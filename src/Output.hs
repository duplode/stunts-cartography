{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Output
    ( writeImageFromTrk
    , writeImageFromRpl
    ) where

import Data.Array
import Control.Arrow ((***))
import Control.Monad (guard)
import Control.Monad.Identity (runIdentity)
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except
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
import Util.ZipConduit
import Replay
import Composition
import qualified Parameters as Pm
import Annotation (annotationDiagram)
import Annotation.Flipbook
import Types.CartoM
import Types.Diagrams

writeImageFromTrk :: (MonadIO m, Functor m)
                  => FilePath -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageFromTrk trkPath =
    liftIO (LB.readFile trkPath)
        >>= writeImageOutput (takeBaseName trkPath)

writeImageFromRpl :: (MonadIO m, Functor m)
                  => FilePath -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageFromRpl rplPath = do
    rplData <- liftIO $ LB.readFile rplPath
    if trackDataHasTheCorrectSize rplData
        then uncurry writeImageOutput $ trkFromRplSimple rplData
        else lift $ throwError "No track data in RPL file."

-- Note that the returned paths do not include the boilerplate prefix.
writeImageOutput :: (MonadIO m, Functor m)
                 => String -> LB.ByteString
                 -> CartoT (ExceptT String m) Pm.PostRenderInfo
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
    fbks <- asks Pm.flipbookSpec
    case fbks of
        [] -> do
            outType <- asks Pm.outputType
            let outRelPath = case outType of
                    PNG -> "stunts-cartography-map.png"
                    SVG -> "stunts-cartography-map.svg"
                    _   -> error "Unsupported output format."
                outFile = tmpDir </> outRelPath

            startTime <- liftIO getCPUTime
            wholeMap <- wholeMapDiagram tiles
            liftIO $ renderCairo outFile (mkWidth renWidth) wholeMap
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
                , Pm.outputRelPath = outRelPath
                , Pm.flipbookRelPath = Nothing
                }

        _ -> do
            startTime <- liftIO getCPUTime
            wholeMap <- wholeMapDiagram tiles

            -- Ignoring the output type, at least for now.
            modify Pm.incrementNumberOfRuns
            fbkRelDir <- createFlipbookDir tmpDir trackName
            let fbkDir = tmpDir </> fbkRelDir

            -- Five digits are enough for Stunts replays of any length.
            let renderPage (ix, pg) = renderCairo
                    (fbkDir </> (printf "%05d.png" ix)) (mkWidth renWidth) pg

            -- fbks is, in effect, mconcat'ed twice (first through
            -- zipFlipbookPages and then through concatFlipbookBackdrops)
            -- to prevent a space leak.
            -- Thanks to laziness, neither the backdrop nor the pages are
            -- actually rendered twice; therefore, this implementation is
            -- acceptable until a more elegant solution, if any, is found.
            liftIO $ mapM_ renderPage $
                zip ([0..] :: [Int]) . map (withEnvelope wholeMap) $
                    zipFlipbookPages fbks

            let backdropRelFile = fbkRelDir </> "backdrop.png"
                backdropFile = tmpDir </> backdropRelFile
                fullBackdrop = concatFlipbookBackdrops fbks <> wholeMap
            liftIO $ renderCairo backdropFile (mkWidth renWidth) fullBackdrop

            nRuns <- gets Pm.numberOfRuns
            let zipRelFile = "flipbook-" ++ show nRuns ++ ".zip"
                zipFile = tmpDir </> zipRelFile
            liftIO $ writeDirContentsZip fbkDir zipFile

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
                , Pm.outputRelPath = backdropRelFile
                , Pm.flipbookRelPath = Just zipRelFile
                }

-- Note that the returned path does not include the boilerplate prefix.
createFlipbookDir :: (MonadIO m) => FilePath -> String -> CartoT m FilePath
createFlipbookDir tmpDir trackName =
    gets Pm.numberOfRuns >>= liftIO . createIt
    where
    dirTemplate = "flipbook-" ++ trackName ++ "-"
    createIt n = do
        let dirPath = dirTemplate ++ show n
        r <- tryJust (guard . isAlreadyExistsError) $
            createDirectory (tmpDir </> dirPath)
        case r of
            Left _  -> createIt (n + 1)
            Right _ -> return dirPath


-- TODO: Possibly generalize the CartoM computations in Composition and below.
wholeMapDiagram :: (Monad m) => [Tile] -> CartoT m (Diagram BEDia)
wholeMapDiagram tiles = mapRWST (return . runIdentity) $ do
    params <- ask
    let minBounds :: (Double, Double)
        minBounds = fromIntegral *** fromIntegral $ Pm.minTileBounds params
        (minX, minY) = minBounds
        deltaBounds = Pm.deltaTileBounds params
        (deltaX, deltaY) = deltaBounds
        adjustPositionForClip = moveOriginBy (r2 minBounds # reflectX # reflectY)
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

--pile' = foldl' beneath mempty
