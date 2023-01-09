{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Output
    ( writeImageFromTrk
    , writeImageFromRpl
    , wholeMapDiagram
    ) where

import Data.Array
import Control.Arrow ((***))
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Monad.Except
import Control.Exception (tryJust)
import System.FilePath (takeBaseName, (</>))
import System.Directory (createDirectory)
import System.IO.Error (isAlreadyExistsError)
import System.CPUTime
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as LB

import Diagrams.Prelude

import Track (Tile, veryRawReadTrack, rawTrackToTileArray, horizonFromRawTrack
    , printHorizon)
import Util.Zip
import Replay
import Composition
import qualified Parameters as Pm
import Annotation (annotationDiagram)
import Annotation.Flipbook
import Types.CartoM
import Util.Diagrams.Backend (OutputType(..), B, renderBE
    , widthConversionFactor)

writeImageFromTrk :: MonadIO m
                  => FilePath -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageFromTrk trkPath = do
    trkData <- liftIO $ LB.readFile trkPath
    writeImageOutput (takeBaseName trkPath) trkData

writeImageFromRpl :: MonadIO m
                  => FilePath -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageFromRpl rplPath = do
    rplData <- liftIO $ LB.readFile rplPath
    if trackDataHasTheCorrectSize rplData
        then uncurry writeImageOutput $ trkFromRplSimple rplData
        else lift $ throwError "No track data in RPL file."

-- Note that the returned paths do not include the boilerplate prefix.
writeImageOutput :: MonadIO m
                 => String -> LB.ByteString
                 -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageOutput trackName trkBS = do
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr

    outType <- asks Pm.outputType
    renWidthInTiles <- (\drawIx -> if drawIx then (2+) else id)
        <$> asks Pm.drawIndices <*> asks (fst . Pm.deltaTileBounds)
    renWidth <- (renWidthInTiles * widthConversionFactor outType *)
        <$> asks Pm.pixelsPerTile
    tmpDir <- asks Pm.temporaryDirectory

    fbks <- asks Pm.flipbookSpec
    -- Whether to render a regular map or an animation flipbook.
    postInfo <- case fbks of
        [] -> do
            startTime <- liftIO getCPUTime

            let outRelPath = case outType of
                    PNG -> "stunts-cartography-map.png"
                    SVG -> "stunts-cartography-map.svg"
                outFile = tmpDir </> outRelPath

            wholeMap <- wholeMapDiagram tiles
            liftIO $ renderBE outFile (mkWidth renWidth) wholeMap
            endTime <- liftIO getCPUTime

            let fullDeltaTime :: Double
                fullDeltaTime = fromIntegral (endTime - startTime) / 10^9
            tell . Pm.logFromString $
                printf "Rendering time (core + output writing): %0.0fms\r\n"
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

            modify Pm.incrementNumberOfRuns
            fbkRelDir <- createFlipbookDir tmpDir trackName
            let fbkDir = tmpDir </> fbkRelDir
                outExt = case outType of
                    PNG -> "png"
                    SVG -> "svg"

                -- Five digits are enough for Stunts replays of any length.
                renderPage (ix, pg) = renderBE
                    (fbkDir </> (printf "%05d.%s" ix outExt))
                    (mkWidth renWidth)
                    pg

            -- fbks is, in effect, mconcat'ed twice (first through
            -- zipFlipbookPages and then through concatFlipbookBackdrops)
            -- to prevent a space leak.
            -- Thanks to laziness, neither the backdrop nor the pages are
            -- actually rendered twice; therefore, this implementation is
            -- acceptable until a more elegant solution, if any, is found.
            liftIO $ mapM_ renderPage $
                zip ([0..] :: [Int]) . map (withEnvelope wholeMap) $
                    zipFlipbookPages fbks

            let backdropRelFile = fbkRelDir </> printf "backdrop.%s" outExt
                backdropFile = tmpDir </> backdropRelFile
                fullBackdrop = concatFlipbookBackdrops fbks <> wholeMap
            liftIO $ renderBE backdropFile (mkWidth renWidth) fullBackdrop

            nRuns <- gets Pm.numberOfRuns
            let zipRelFile = "flipbook-" ++ show nRuns ++ ".zip"
                zipFile = tmpDir </> zipRelFile
            liftIO $ writeDirContentsZip fbkDir zipFile

            endTime <- liftIO getCPUTime
            tell . Pm.logFromString $ "Flipbook rendering complete.\r\n"
            let fullDeltaTime :: Double
                fullDeltaTime = fromIntegral (endTime - startTime) / 10^12
            tell . Pm.logFromString $
                printf "Rendering time (core + output writing): %0.3fs\r\n"
                    fullDeltaTime

            return Pm.PostRenderInfo
                { Pm.renderedTrackHorizon = horizon
                , Pm.trackName = trackName
                , Pm.trackData = trkBS
                , Pm.outputRelPath = backdropRelFile
                , Pm.flipbookRelPath = Just zipRelFile
                }

    tell . Pm.logFromString $ printf "Track name: %s\r\n" (Pm.trackName postInfo)
    tell . Pm.logFromString $ printf "Scenery: %s\r\n"
        (printHorizon (Pm.renderedTrackHorizon postInfo))

    return postInfo

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
wholeMapDiagram :: Monad m => [Tile] -> CartoT m (Diagram B)
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
