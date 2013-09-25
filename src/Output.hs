{-# LANGUAGE NoMonomorphismRestriction #-}
module Output
    ( writeImageFromTrk
    , writeImageFromRpl
    ) where

import Data.Array
import Control.Monad (guard)
import Control.Monad.Identity (runIdentity)
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Error
import Control.Exception (tryJust)
import System.FilePath (takeBaseName, (</>))
import System.Directory (createDirectory)
import System.IO.Error (isAlreadyExistsError)
import System.CPUTime
import Text.Printf (printf)
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Core
import Track (Tile, veryRawReadTrack, rawTrackToTileArray, horizonFromRawTrack)
import qualified Util.ByteString as LB
import Util.Misc
import Replay
import Composition
import qualified Parameters as Pm
import Annotation (annotationDiagram)
import Annotation.Flipbook
import Types.CartoM
import Types.Diagrams

writeImageFromTrk :: FilePath -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writeImageFromTrk trkPath =
    liftIO (LB.readFile trkPath)
        >>= writeImageOutput (takeBaseName trkPath)

writeImageFromRpl :: FilePath -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writeImageFromRpl rplPath = do
    rplData <- liftIO $ LB.readFile rplPath
    if trackDataHasTheCorrectSize rplData
        then uncurry writeImageOutput $ trkFromRplSimple rplData
        else lift $ throwError "No track data in RPL file."

writeImageOutput :: String -> LB.ByteString
               -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writeImageOutput trackName trkBS = do
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr

    renWidthInTiles <- pure (\drawIx -> if drawIx then (2+) else id)
        <*> asks Pm.drawIndices <*> asks (fst . Pm.deltaTileBounds)
    renWidth <- pure (renWidthInTiles *) <*> asks Pm.pixelsPerTile
    tmpDir <- asks Pm.temporaryDirectory

    -- Whether to render a regular map or an animation flipbook.
    mFbk <- asks Pm.flipbookSpec
    case mFbk of
        Nothing -> do
            outType <- asks Pm.outputType
            let outRelPath = case outType of
                    PNG -> "stunts-cartography-map.png"
                    SVG -> "stunts-cartography-map.svg"
                    _   -> error "Unsupported output format."
                outFile = tmpDir </> outRelPath

            startTime <- liftIO getCPUTime
            wholeMap <- wholeMapDiagram tiles
            liftIO $ renderCairo outFile (Width renWidth) wholeMap
            endTime <- liftIO getCPUTime

            let fullDeltaTime :: Double
                fullDeltaTime = fromIntegral (endTime - startTime) / 10^9
            tell . Pm.logFromList $
                printf "Rendering time (core + output writing): %0.0fms.\r\n"
                    fullDeltaTime

            modify Pm.incrementNumberOfRuns
            return Pm.PostRenderInfo
                { Pm.renderedTrackHorizon = horizon
                , Pm.trackName = trackName
                , Pm.trackData = trkBS
                , Pm.outputPath = outFile
                }

        Just fbk -> do
            startTime <- liftIO getCPUTime
            wholeMap <- wholeMapDiagram tiles
            let fbk' = underlayFlipbook wholeMap fbk
                (backdrop, pages) = renderFlipbook fbk'

            -- Ignoring the output type, at least for now.
            modify Pm.incrementNumberOfRuns
            fbkDir <- createFlipbookDir tmpDir trackName

            let backdropFile = fbkDir </> "backdrop.png"
                -- Five digits are enough for Stunts replays of any length.
                renderPage (ix, pg) = liftIO $ renderCairo
                    (fbkDir </> (printf "%05d.png" ix)) (Width renWidth) pg
            liftIO $ renderCairo backdropFile (Width renWidth) backdrop
            mapM renderPage $ zip ([0..] :: [Int]) pages

            endTime <- liftIO getCPUTime
            tell . Pm.logFromList $ "Flipbook rendering complete.\r\n"
            let fullDeltaTime :: Double
                fullDeltaTime = fromIntegral (endTime - startTime) / 10^12
            tell . Pm.logFromList $
                printf "Rendering time (core + output writing): %0.3fs.\r\n"
                    fullDeltaTime

            return Pm.PostRenderInfo
                { Pm.renderedTrackHorizon = horizon
                , Pm.trackName = trackName
                , Pm.trackData = trkBS
                , Pm.outputPath = backdropFile
                }

createFlipbookDir :: (MonadIO m) => FilePath -> String -> CartoT m FilePath
createFlipbookDir tmpDir trackName =
    gets Pm.numberOfRuns >>= liftIO . createIt
    where
    dirTemplate = tmpDir </> ("flipbook-" ++ trackName ++ "-")
    createIt n = do
        let dirPath = dirTemplate ++ show n
        r <- tryJust (guard . isAlreadyExistsError) $
            createDirectory dirPath
        case r of
            Left _  -> createIt (n + 1)
            Right _ -> return dirPath


-- TODO: Possibly generalize the CartoM computations in Composition and below.
wholeMapDiagram :: (Monad m) => [Tile] -> CartoT m (Diagram BEDia R2)
wholeMapDiagram tiles = mapRWST (return . runIdentity) $ do
    params <- ask
    let minBounds = Pm.minTileBounds params
        (minX, minY) = minBounds
        deltaBounds = Pm.deltaTileBounds params
        (deltaX, deltaY) = deltaBounds
        adjustPositionForClip = moveOriginBy (r2 minBounds # reflectX # reflectY)
        clipRect :: Path R2
        clipRect = unitSquare # scaleX deltaX # scaleY deltaY
            # alignBL # adjustPositionForClip
    wholeMap <- renderMap tiles
    indices <- renderIndicesIfRequired
    return $
        (mconcat . map annotationDiagram $ Pm.annotationSpecs params)
        <>
        (indices # adjustPositionForClip)
        <>
        (
            (if Pm.drawGridLines params then gridLines else mempty)
            <>
            wholeMap
        )
        # clipBy clipRect
        # withEnvelope clipRect

