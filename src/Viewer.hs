{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Viewer
    ( subMain
    , Options (..)
    , opts
    ) where

import Control.Monad
import qualified Control.Monad.RWS as RWS
import Control.Monad.Except
import Control.Exception (catch, SomeException)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.List (find)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory
    (doesFileExist, doesDirectoryExist, getFileSize ,getCurrentDirectory)
import System.FilePath ((</>), takeExtension, addExtension,takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified Options.Applicative as Opts
import Data.Char (toUpper)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Ext.Flexbox
import qualified Clay as Clay hiding (Clay.Flexbox)
import qualified Clay.Flexbox as Flex

import Util.Misc (trimFracPart)
import Util.Diagrams.Backend (forkRender
    , OutputType(..), defaultOutputType, alternativeOutputTypes)
import Util.Reactive.Threepenny (concatE, union)
import Util.Threepenny.Flexbox
import Output
import Track (Horizon(..), terrainTrkSimple)
import qualified Parameters as Pm
import Annotation (Annotation)
import Annotation.Flipbook (SomeFlipbook)
import Annotation.Parser (parseAnnotations, parseFlipbook)
import Types.CartoM
import Paths (versionString, isPortableBuild, getDataDir)
import qualified Widgets.BoundedInput as BI
import qualified Widgets.FilePathPicker as FPP
import Util.Threepenny.Alertify
import Util.Threepenny.JQueryAutocomplete
import Util.Threepenny (value_Text, selectionChange', removeAttr
    , checkboxUserModel)

subMain :: Options -> IO ()
subMain o = withSystemTempDirectory "stunts-cartography-" $ \tmpDir -> do
    let Options { portNumber = port, initialDirectory = mInitDir } = o
    -- Defaulting to the parent of the current directory is convenient
    -- if the executable is at a subdirectory of the Stunts directory.
    initDir <- case mInitDir of
        Just d -> return d
        Nothing -> takeDirectory <$> getCurrentDirectory
    staticDir <- (</> "wwwroot") <$> getDataDir
    consoleGreeting $! port
    startGUI defaultConfig
        { jsPort = Just port
        , jsCustomHTML = Just "index.html"
        , jsStatic = Just staticDir
        } $ setup initDir tmpDir

data Options = Options
    { portNumber :: Int
    , initialDirectory :: Maybe FilePath
    }

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> Opts.option Opts.auto
        ( Opts.short 'p'
        <> Opts.long "port"
        <> Opts.help "Port number"
        <> Opts.showDefault
        <> Opts.value 8023
        <> Opts.metavar "INT"
        )
    <*> Opts.option (Opts.eitherReader (Right . Just))
        ( Opts.short 'd'
        <> Opts.long "directory"
        <> Opts.help "Initial directory"
        <> Opts.showDefaultWith (const "..")
        <> Opts.value Nothing
        <> Opts.metavar "DIRECTORY"
        )

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts
    ( Opts.fullDesc
    <> Opts.progDesc "Generate and annotate Stunts track maps"
    )

consoleGreeting :: Int -> IO ()
consoleGreeting port = do
    putStrLn $ printf "Welcome to %s." formattedVersionString
    putStrLn $ printf "Open your web browser and navigate to localhost:\
                      \%d to begin." port
    putStrLn ""
    when isPortableBuild $ do
        putStrLn "This is a portable build."
        putStrLn "If the interface doesn't load, check \
                 \if there is a wwwroot directory with"
        putStrLn "auxiliary files of the application \
                 \in the directory of the executable."
        putStrLn ""

formattedVersionString :: String
formattedVersionString = printf "Stunts Cartography %s(Viewer)"
    (maybe "" (++ " ") versionString)

setup :: FilePath -> FilePath -> Window -> UI ()
setup initDir tmpDir w = void $ do

    return w # set title "Stunts Cartography (Viewer)"
    --UI.addStyleSheet w "viewer.css"
    --autocompleteSetup w "static/lib/"
    --alertifySetup w "static/lib/"

    -- Output type selection.
    -- defaultOutputType and alternativeOutputTypes depend on the
    -- diagrams backend, so that only available types will be
    -- presented.
    selOutput <-
        UI.select # set UI.name "output-format-select"
            # set UI.id_ "output-format-select" #+
                ([ UI.option # set UI.selected True
                        #+ [string (show defaultOutputType)]]
                    ++ map (\ot -> UI.option #+ [string (show ot)])
                        alternativeOutputTypes)
            # if null alternativeOutputTypes
                then set UI.enabled False
                else id

    let eSelOutput = selectionChange' selOutput
    bOutType <- (intToOutputType . fromMaybe (-1) <$>)
        <$> (Just 0 `stepper` eSelOutput)

    -- Base directory and file selection, with autocompletion.
    fppPicker <- FPP.new
    bPickedPath <- FPP.userModel
        FPP.PickedPath
            { baseDir = initDir
            , relativePath = ""
            }
        fppPicker

    let bBaseDir = FPP.baseDir <$> bPickedPath
        bRelPath = FPP.relativePath <$> bPickedPath

    bidPxPerTile <- BI.new (8, 128)
    bPxPerTile   <- Pm.pixelsPerTile def `BI.userModel` bidPxPerTile

    -- Map bounds, grid and indices.

    let (defMinX, defMaxX) = Pm.xTileBounds def
        (defMinY, defMaxY) = Pm.yTileBounds def

        styleBoundsInput = BI.setTextInputWidth 2
            . BI.formatBoundsCaption (const "")

    biiMinX <- BI.new (0, 29) # styleBoundsInput
    bMinX   <- defMinX `BI.userModel` biiMinX
    biiMaxX <- BI.new (0, 29) # styleBoundsInput
    bMaxX   <- defMaxX `BI.userModel` biiMaxX
    biiMinY <- BI.new (0, 29) # styleBoundsInput
    bMinY   <- defMinY `BI.userModel` biiMinY
    biiMaxY <- BI.new (0, 29) # styleBoundsInput
    bMaxY   <- defMaxY `BI.userModel` biiMaxY

    let ensureBoundOrder bounds@(z, w) = if z > w then (w, z) else bounds
        bBoundsX = ensureBoundOrder <$> (pure (,) <*> bMinX <*> bMaxX)
        bBoundsY = ensureBoundOrder <$> (pure (,) <*> bMinY <*> bMaxY)

    chkDrawGrid <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
            # set UI.id_ "grid-lines-chk"
    bDrawGrid <- Pm.drawGridLines def `checkboxUserModel` chkDrawGrid

    chkDrawIndices <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
            # set UI.id_ "grid-indices-chk"
    bDrawIndices <- Pm.drawIndices def `checkboxUserModel` chkDrawIndices

    chkTransparentBg <-
        UI.input # set UI.type_ "checkbox" # set UI.name "transparent-bg-chk"
            # set UI.id_ "transparent-bg-chk"
    bTransparentBg <- Pm.transparentBg def `checkboxUserModel` chkTransparentBg

    chkTwoToneTerrain <-
        UI.input # set UI.type_ "checkbox" # set UI.name "two-tone-chk"
            # set UI.id_ "two-tone-chk"
    bTwoToneTerrain <- Pm.twoToneTerrain def `checkboxUserModel` chkTwoToneTerrain

    -- Preset selection and ratio field initialization.
    let presetDefAndSetter :: (Pm.RenderingParameters -> a)
                           -> Event (Pm.RenderingParameters)
                           -> (a, Event (a -> a))
        presetDefAndSetter fParam eParams =
            (fParam def, const . fParam <$> eParams)

        -- Note the result of applying trimFracPart here is used to
        -- initialise parameters, and not just to format text for the
        -- interface.
        presetDefAndSetterD fParam =
            (\(x, e) -> (trimFracPart 3 x, (trimFracPart 3 .) <$> e))
                . presetDefAndSetter fParam

    selPreset <-
        UI.select # set UI.name "style-preset-select"
            # set UI.id_ "style-preset-select"
            # set UI.title__ "Use the \"Set\" button to conifrm the choice" #+
                [ UI.option # set UI.selected True #+ [string "Default"]
                , UI.option #+ [string "Wider track"]
                , UI.option #+ [string "Sloping ramps"]
                , UI.option #+ [string "Traditional"]
                ]

    let eSelPreset = selectionChange' selPreset
    bPreset <- (intToPresetRenderingParams . fromMaybe (-1) <$>)
        <$> (Just 0 `stepper` eSelPreset)

    btnPreset <- UI.button #. "button" #+ [string "Set"]

    let ePreset = bPreset <@ UI.click btnPreset

    -- Rendering ratios
    let ratioModel fParam bi = do
            let (defRatio, eRatio) = presetDefAndSetterD fParam ePreset
            -- TODO: The refresh presumably only matters if eRatio is
            -- somehow fired while the input is being edited. It might
            -- make sense to have a closer look at the need for it.
            eRatio' <- BI.withRefresh bi eRatio
            BI.simpleModel defRatio eRatio' bi

    bidRoadW       <- BI.new (0.1, 0.5)
    bRoadW         <- Pm.roadWidth `ratioModel` bidRoadW
    bidBridgeH     <- BI.new (0, 0.5)
    bBridgeH       <- Pm.bridgeHeight `ratioModel` bidBridgeH
    bidBridgeRelW  <- BI.new (1, 3)
    bBridgeRelW    <- Pm.bridgeRelativeWidth `ratioModel` bidBridgeRelW
    bidBankingRelH <- BI.new (0.25, 1)
    bBankingRelH   <- Pm.bankingRelativeHeight `ratioModel` bidBankingRelH

    -- Rendering parameters.
    -- Note that the annotations are parsed in a separate step.

    let bRenParams = pure def {Pm.temporaryDirectory = tmpDir}
            <**> ((\x -> \p -> p {Pm.baseDirectory = x}) <$> bBaseDir)
            <**> ((\x -> \p -> p {Pm.roadWidth = x}) <$> bRoadW)
            <**> ((\x -> \p -> p {Pm.bridgeHeight = x}) <$> bBridgeH)
            <**> ((\x -> \p -> p {Pm.bridgeRelativeWidth = x}) <$> bBridgeRelW)
            <**> ((\x -> \p -> p {Pm.bankingRelativeHeight = x})
                <$> bBankingRelH)
            <**> ((\x -> \p -> p {Pm.transparentBg = x}) <$> bTransparentBg)
            <**> ((\x -> \p -> p {Pm.twoToneTerrain = x}) <$> bTwoToneTerrain)
            <**> ((\x -> \p -> p {Pm.pixelsPerTile = x}) <$> bPxPerTile)
            <**> ((\x -> \p -> p {Pm.xTileBounds = x}) <$> bBoundsX)
            <**> ((\x -> \p -> p {Pm.yTileBounds = x}) <$> bBoundsY)
            <**> ((\x -> \p -> p {Pm.drawGridLines = x}) <$> bDrawGrid)
            <**> ((\x -> \p -> p {Pm.drawIndices = x}) <$> bDrawIndices)
            <**> ((\x -> \p -> p {Pm.outputType = x}) <$> bOutType)

    -- The go button.

    btnGo <- UI.button #. "go-button" #+ [string "Draw map"]
    let eGo = UI.click btnGo

    -- Misc. interesting elements

    txaLog <-
        UI.textarea # set UI.id_ "log-text"
            # set UI.cols "72" # set UI.rows "6"

    lnkTrk <-
        UI.a # set UI.id_ "save-trk-link" # set UI.target "_blank" #+
            [string "track"]
    lnkTerrTrk <-
        UI.a # set UI.id_ "save-terrain-link" # set UI.target "_blank" #+
            [string "terrain"]
    lnkFlipbook <-
        UI.a # set UI.id_ "save-flipbook" # set UI.target "_blank" #+
            [string "flipbook"]

    txaAnns <-
        UI.textarea # set UI.name "ann-input"
            # set UI.id_ "ann-input"
            # set UI.cols "25" # set UI.rows "5"

    txaFlipbook <-
        UI.textarea # set UI.name "flipbook-input"
            # set UI.id_ "flipbook-input"
            # set UI.cols "25" # set UI.rows "5"

    imgMap <- UI.img # set UI.id_ "track-map" # set UI.src "static/images/welcome.png"

    -- Assembling the interface HTML.

    theBody <- getBody w # set UI.id_ "the-body" #. "blank-horizon"
        # setFlex parentProps #+
        [ UI.div # set UI.id_ "left-bar"
            # setFlex (flexDirection Clay.column)
            # setFlex (flexGrow 1)
            #+
            [ divVertFlex #+
                [ rowFlex #+
                    [ subRowFlex #+ [element btnGo]
                    , subRowFlex #+ [string "as ", element selOutput]
                    ]
                , rowFlex #+
                    [ subRowFlex #+ [UI.span #. "ui-icon ui-icon-save", string " Save:"]
                    , subRowFlex #+
                        [ element lnkTrk, string " - "
                        , element lnkTerrTrk, string " - "
                        , element lnkFlipbook
                        ]
                    ]
                ]
            , element fppPicker
            , divVertFlex #+
                [ rowFlex #+ [string "Style presets:"]
                , rowFlex #+ [element selPreset, element btnPreset]
                , rowFlex #+ [string "Road width:", element bidRoadW]
                , rowFlex #+ [string "Bridge height:", element bidBridgeH]
                , rowFlex #+ [string "Bridge % width:", element bidBridgeRelW]
                , rowFlex #+ [string "Bank. % height:", element bidBankingRelH]
                ]
            , divVertFlex #+
                [ rowFlex #+ [string "Two-tone terrain?", element chkTwoToneTerrain]
                , rowFlex #+ [string "Transparent low ground?", element chkTransparentBg]
                , rowFlex #+ [string "Grid?", element chkDrawGrid]
                , rowFlex #+ [string "Indices?", element chkDrawIndices]
                ]
            , divVertFlex #+
                [ rowFlex # modifyFlex (alignItems Clay.center) #+
                    [ divVertFlex # modifyFlex (alignItems Clay.center) #+
                        [string "Map bounds:", string "(0 - 29)"]
                    , subRowFlex # modifyFlex (alignItems Clay.center) #+
                        [ divVertFlex # modifyFlex (alignItems Clay.flexEnd) #+
                            [ UI.span #. "ui-icon ui-icon-caret-1-nw"
                            , element biiMinX
                            , UI.span #. "ui-icon ui-icon-caret-1-sw"
                            ]
                        , divVertFlex # modifyFlex (alignItems Clay.center) #+
                            [ element biiMaxY
                            , element biiMinY
                            ]
                        , divVertFlex # modifyFlex (alignItems Clay.flexStart) #+
                            [ UI.span #. "ui-icon ui-icon-caret-1-ne"
                            , element biiMaxX
                            , UI.span #. "ui-icon ui-icon-caret-1-se"
                            ]
                        ]
                    ]
                ]
            , rowFlex #+
                [ string "Pixels per tile:"
                , element bidPxPerTile
                ]
            , divVertFlex #+
                [ rowFlex #+
                    [ string "Annotations:"
                    , UI.a # set UI.text "Help"
                        # set UI.href ("static/annotations-help.html")
                        # set UI.target "_blank"
                    ]
                , element txaAnns
                ]
            , divVertFlex #+
                [ rowFlex #+
                    [ string "Flipbook:"
                    , UI.a # set UI.text "Help"
                        # set UI.href ("static/annotations-help.html#creating-flipbooks")
                        # set UI.target "_blank"
                    ]
                , element txaFlipbook
                ]
            ]
        , UI.div # set UI.id_ "main-wrap"
            # setFlex (flexDirection Clay.column) # setFlex childProps
            #+
            [ element imgMap
            , divVertFlex #+
                [ string "Log:"
                , element txaLog
                ]
            ]
        ]

    -- Main action validation.
    let validateAndIdentifyExtension :: MonadIO m => FilePath -> ExceptT String m String
        validateAndIdentifyExtension trkPath = do
            trkExists <- liftIO $ doesFileExist trkPath
            unless trkExists . void $
                throwError "File does not exist."

            let fileExt = map toUpper $ takeExtension trkPath
                extIsKnown = fileExt == ".TRK" || fileExt == ".RPL"
            unless extIsKnown . void $
                throwError "Bad file extension: should be .TRK or .RPL, in upper or lower case."

            fileSize <- liftIO $ getFileSize trkPath
            let trkSizeIsCorrect = fileSize >= 1802 && fileSize <= 13802
                badTRKSize = fileExt == ".TRK" && not trkSizeIsCorrect
            when badTRKSize . void $
                throwError "Bad file size: TRK files must have between 1802 and 13802 bytes."
            let rplSizeIsCorrect = fileSize >= 1826 && fileSize <= 13828
                badRPLSize = fileExt == ".RPL" && not rplSizeIsCorrect
            when badRPLSize . void $
                throwError "Bad file size: RPL files must have between 1826 and 13828 bytes."
            return fileExt

    -- Main action outcome handlers. Ideally, these handlers should
    -- only deal with what is inconvenient to do through the event
    -- network. Right now, that amounts to:
    --
    -- - Everything that uses loadFile or loadDirectory, so that I
    --   don't have to worry with what unsafeMapIO would do with them;
    --   and
    --
    -- - The horizon background, so that it changes synchronously with
    --   the map image.

    -- TODO: We probably don't need to pass the rendering state here.
    -- ExceptT failures always happen before any rendering is done, so
    -- the rendering state isn't changed when they happen.
    let handleRenderingFailure :: (Pm.RenderingState, Pm.RenderingLog) -> UI ()
        handleRenderingFailure _ = do
            element theBody #. "blank-horizon"
            element imgMap # set UI.src "static/images/failure.png"
            mapM_ (removeAttr "href" . element)
                [lnkTrk, lnkTerrTrk, lnkFlipbook]

        -- We pass the rendering parameters here instead of relying on
        -- bRenParams to make sure we are using the same parameters
        -- used for the rendering. bRenParams is affected by the
        -- interface controls.
        handleRenderingSuccess
            :: (Pm.RenderingParameters, Pm.PostRenderInfo
                , Pm.RenderingState, Pm.RenderingLog)
            -> UI ()
        handleRenderingSuccess (params, postRender, _, _) = do
            -- TODO: Integrate as much as we can of this to the event
            -- network.
            when (isJust $ Pm.flipbookRelPath postRender) $ alertifySuccess
                "Flipbook ready! Use the flipbook link on the left to save it."
            element theBody #. horizonClass (Pm.renderedTrackHorizon postRender)
            let outType = Pm.outputType params
            trackImage <- loadTrackImage outType
                (tmpDir </> Pm.outputRelPath postRender)
            trkUri <- loadTmpTrk tmpDir postRender
            terrainUri <- loadTmpTerrainTrk tmpDir postRender
            mFlipbookUri <- maybe (return Nothing)
                ((Just <$>) . loadFile "application/zip" . (tmpDir </>))
                (Pm.flipbookRelPath postRender)
            element imgMap # set UI.src trackImage
            element lnkTrk # set UI.href trkUri
            element lnkTerrTrk # set UI.href terrainUri
            void $ maybe (element lnkFlipbook # removeAttr "href")
                ((element lnkFlipbook #) . set UI.href) mFlipbookUri

    -- Outcomes of the main action.
    (eRenderingFailure, notifyRenderingFailure) <- liftIO newEvent
    onEvent eRenderingFailure handleRenderingFailure

    (eRenderingSuccess, notifyRenderingSuccess) <- liftIO newEvent
    onEvent eRenderingSuccess handleRenderingSuccess

    -- Output from the main action, input for the next run.
    let eRenState = ((\(_, _, st, _) -> st) <$> eRenderingSuccess)
            `union` (fst <$> eRenderingFailure)
    bRenState <- def `stepper` eRenState

    -- Log handling.
    let eAppendToLog = ((\(_, _, _, w) -> w) <$> eRenderingSuccess)
            `union` (snd <$> eRenderingFailure)

        appendLnTo line log = log <> line <> Pm.logFromString "\r\n"

        ePutLnLog = appendLnTo <$> eAppendToLog
        -- The log is cleared at the beginning of the chain.
        eClearLog = const mempty <$ eGo

        initialLogContents = Pm.logFromString $
            maybe "" (printf "Program version: %s\r\n") versionString
    -- Note that if an eClearLog is simultaneous with an ePutLnLog the line
    -- will not be appended.
    bLogContents <- initialLogContents `accumB` (eClearLog `union` ePutLnLog)
    element txaLog # sink value_Text (Pm.logToText <$> bLogContents)

    -- Draw button status.
    bAllClear <- True `stepper` foldr union never
        [False <$ eGo, True <$ eRenderingSuccess, True <$ eRenderingFailure]
    element btnGo # sink UI.enabled bAllClear

    -- The main action proper.

    -- TODO: It should be possible to untangle some of what follows by
    -- making better use of the event network.
    let runRenderMap :: (Pm.RenderingParameters, Pm.RenderingState) -> UI ()
        runRenderMap (params, st) = do

            trkRelPath <- currentValue bRelPath
            let basePath = Pm.baseDirectory params
                trkPath = basePath </> trkRelPath

            eitImgWriter <- runExceptT $ do

                -- Note that the trkPath file need not necessarily be
                -- a TRK. It is also acceptable for it to be a RPL.
                fileExt <- validateAndIdentifyExtension trkPath

                -- Decide on input format.
                let imgWriter :: FilePath -> CartoT (ExceptT String IO) Pm.PostRenderInfo
                    imgWriter = case fileExt of
                        ".TRK" -> writeImageFromTrk
                        ".RPL" -> writeImageFromRpl
                        _      -> error "Unrecognized input extension."

                return imgWriter

            case eitImgWriter of
                Left errorMsg -> liftIO $
                    notifyRenderingFailure (st, Pm.logFromString errorMsg)

                Right imgWriter -> do
                    -- Parse annotations and render the map.
                    -- Ideally, we'd give parseAnns a signature which expressed how
                    -- it can't affect the rendering state. That, however, would
                    -- require either an orphan MonadUI instance or making CartoT
                    -- a newtype. The latter might end up happening eventually.
                    let parseAnns :: CartoT UI ([Annotation], [SomeFlipbook])
                        parseAnns = do
                            anns <- lift (txaAnns # get value) >>= parseAnnotations
                            fbks <- lift (txaFlipbook # get value) >>= parseFlipbook
                            lift $ unless (null fbks) $ alertifyLog'
                                "Flipbook rendering usually takes a few minutes. Please stand by..."
                                StandardLog 10000
                            return (anns, fbks)

                    -- We know the computation can't possibly change the state, but
                    -- runRWST has no awareness of that.
                    ((anns, fbks), st', w) <- RWS.runRWST parseAnns params st
                    -- TODO: It would probably make sense to fire an event to update
                    -- bRenParams with the parsed annotations, should we ever need
                    -- to use them anywhere else.
                    let params' = params
                            { Pm.annotationSpecs = anns
                            , Pm.flipbookSpec = fbks
                            }

                    -- We don't bail out on an annotation parsing failure. That
                    -- makes sense in at least some situations. For instance, if an
                    -- annotation syntax error is made, it is not unreasonable to
                    -- want to see the base map as it is fixed.
                    --
                    -- We re-tell w rather than using appendToLog right here to
                    -- avoid a stray newline in the output.

                    -- The monadic layer discontinuties here all have to do with the
                    -- need for the crucial, potentially long running computation
                    -- that follows to happen in IO rather than UI, so that we can
                    -- give it to forkOS and stop it from blocking the event
                    -- processing queue.
                    --
                    -- When the program is built with the diagrams-cairo backend
                    -- forkRender is forkOS rather than forkIO. That is so because
                    -- cairo uses thread-local state. See
                    -- https://stackoverflow.com/q/41485126
                    -- and https://stackoverflow.com/q/25726017
                    let goCarto :: CartoT (ExceptT String IO) Pm.PostRenderInfo
                        goCarto = do
                            RWS.tell w
                            imgWriter trkPath

                    liftIO . void . forkRender $ do
                        eitResult <- runExceptT $ RWS.runRWST goCarto params' st'
                        case eitResult of
                            Left errorMsg ->
                                notifyRenderingFailure (st', Pm.logFromString errorMsg)
                            Right (postRender, st'', w') ->
                                notifyRenderingSuccess (params', postRender, st'', w')

    -- Collecting the parameters and firing the main action.
    mdo
        -- Sampling with eGo ensures we are using the same parameters
        -- given to the rendering computation.
        let eRenParams = bRenParams <@ eGo

        -- If the element style parameters were changed, clear the
        -- caches. Note that the checks here also cover the terrain
        -- style parameters, and that both caches are cleared.
        let (eRenParamsDiffEStyle, eRenParamsSameEStyle) = split $
                (\es -> \p -> if Pm.toElemStyle p /= es
                    then Left p else Right p) <$> bRenEStyle <@> eRenParams

            -- The immediate trigger of the main action.
            eParamsAndStateAfterEStyleCheck =
                (flip (,) . Pm.clearTerrainCache . Pm.clearElementCache
                    <$> bRenState
                    <@> eRenParamsDiffEStyle)
                `union` (flip (,) <$> bRenState
                    <@> eRenParamsSameEStyle)

            eRenEStyle = Pm.toElemStyle . fst
                <$> eParamsAndStateAfterEStyleCheck

        -- Element style used in the *previous* run.
        -- Worth pointing out that we have no reason to care what
        -- it is before the first rendering.
        bRenEStyle <- Pm.toElemStyle def `stepper` eRenEStyle

        -- Firing the main action.
        onEvent eParamsAndStateAfterEStyleCheck runRenderMap

loadTmpTrkBase :: (String -> String) -> (LB.ByteString -> LB.ByteString)
               -> FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTrkBase fName fTrk tmpDir postRender = do
    let trkName = "_" ++ fName (Pm.trackName postRender)
        tmpTrkPath = addExtension (tmpDir </> trkName) ".TRK"
    liftIO $ LB.writeFile tmpTrkPath . fTrk $ Pm.trackData postRender
    loadFile "application/octet-stream" tmpTrkPath

loadTmpTrk :: FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTrk = loadTmpTrkBase id id

loadTmpTerrainTrk :: FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTerrainTrk =
    loadTmpTrkBase ((++ "-T") . take 6) terrainTrkSimple

horizonClass :: Horizon -> String
horizonClass horizon = case horizon of
    Desert   -> "desert-horizon"
    Alpine   -> "alpine-horizon"
    City     -> "city-horizon"
    Country  -> "country-horizon"
    Tropical -> "tropical-horizon"
    _        -> "unknown-horizon"

loadTrackImage :: OutputType -> FilePath -> UI String
loadTrackImage outType outPath = case outType of
    PNG -> loadFile "image/png" outPath
    SVG -> loadFile "image/svg+xml" outPath

intToOutputType :: Int -> OutputType
intToOutputType n = case n of
    0 -> defaultOutputType
    _ -> case find ((n ==) . fst) altAssocs of
        Just (_, ot) -> ot
        _ -> error "Viewer.intToOutputType: out of bounds"
    where
    altAssocs = zip [1..] alternativeOutputTypes

-- The order in the case statement matches that in the style-preset-select.
intToPresetRenderingParams :: Int -> Pm.RenderingParameters
intToPresetRenderingParams n = case n of
    0 -> Pm.defaultRenderingParameters
    1 -> Pm.widerRoadsRenderingParameters
    2 -> Pm.slopingRampsRenderingParameters
    3 -> Pm.classicRenderingParameters
    _ -> error "Unknown preset."
