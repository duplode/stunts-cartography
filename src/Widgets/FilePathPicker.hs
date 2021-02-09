{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
module Widgets.FilePathPicker
    ( FilePathPicker
    -- Construction
    , new
    -- Model definition and setup
    , arrangeModel
    , userModel
    -- Typing events
    , userBaseDirChange
    , userRelativePathChange
    -- Blurring events
    , autocompleteBaseDirChange
    , autocompleteRelativePathChange
    -- Other events
    , userMoveUp
    -- Result type
    , PickedPath(..)
    ) where

import Data.List (sort)
import System.Directory
import System.Directory.Extra (listDirectories, listFiles)
import System.FilePath
import Control.Monad
import Data.Char

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Ext.Flexbox
import qualified Clay.Flexbox as Flex

import Util.Reactive.Threepenny
import Util.Threepenny.JQueryAutocomplete
import Util.Threepenny.Flexbox

data PickedPath = PickedPath
    { baseDir :: FilePath
    , relativePath :: FilePath
    }
    deriving (Eq, Ord, Show)

data FilePathPicker = FilePathPicker
    { _itxBaseDir :: Element
    , _itxRelativePath :: Element
    , _btnUp :: Element
    , _btnHome :: Element
    , _divWrapper :: Element

    , _userBaseDirChange :: Event FilePath
    , _userRelativePathChange :: Event FilePath
    , _autocompleteBaseDirChange :: Event FilePath
    , _autocompleteRelativePathChange :: Event FilePath
    , _userMoveUp :: Event ()
    , _userHome :: Event ()
    }

-- TODO: As things stand, there might not be much point in exposing these.
userBaseDirChange :: FilePathPicker -> Event FilePath
userBaseDirChange = _userBaseDirChange

userRelativePathChange :: FilePathPicker -> Event FilePath
userRelativePathChange = _userRelativePathChange

autocompleteBaseDirChange ::  FilePathPicker -> Event FilePath
autocompleteBaseDirChange = _autocompleteBaseDirChange

autocompleteRelativePathChange :: FilePathPicker -> Event FilePath
autocompleteRelativePathChange = _autocompleteRelativePathChange

userMoveUp :: FilePathPicker -> Event ()
userMoveUp = _userMoveUp

userHome :: FilePathPicker -> Event ()
userHome = _userHome

instance Widget FilePathPicker where
    getElement = _divWrapper

new :: UI FilePathPicker
new = do

    _itxBaseDir <-
        UI.input # set UI.type_ "text"
            #. "file-path-picker-base-path"
            # setFlex childProps { cFlexGrow = 1 }

    _itxRelativePath <-
        UI.input # set UI.type_ "text"
            #. "file-path-picker-relative-path"
            # setFlex childProps { cFlexGrow = 1 }

    _btnUp <-
        UI.button
            #. "file-path-picker-button-up"
            # set UI.title__ "Switch to parent directory"
            #+ [ UI.span #. "ui-icon ui-icon-arrow-1-n" ]

    _btnHome <-
        UI.button
            #. "file-path-picker-button-home"
            # set UI.title__ "Switch to home directory"
            #+ [ UI.span #. "ui-icon ui-icon-home" ]

    let _userBaseDirChange = UI.valueChange _itxBaseDir
        _userRelativePathChange = UI.valueChange _itxRelativePath
        _autocompleteBaseDirChange = autocompleteValueChange _itxBaseDir
        _autocompleteRelativePathChange = autocompleteValueChange _itxRelativePath
        _userMoveUp = UI.click _btnUp
        _userHome = UI.click _btnHome

    _strBasePathCaption <- string "Base path: "
        #. "file-path-picker-caption"

    _strRelativePathCaption <- string "TRK / RPL relative path:"
        #. "file-path-picker-caption"

    -- TODO: Make the captions customisable.
    _divWrapper <- divVertFlex #. "file-path-picker" #+
        [ rowFlex #+
            [ rowFlex #+ [element _strBasePathCaption]
            , subRowFlex #+ [element _btnUp, element _btnHome]
            ]
        , rowFlex #+ [element _itxBaseDir]
        , rowFlex #+ [element _strRelativePathCaption]
        , rowFlex #+ [element _itxRelativePath]
        ]

    return FilePathPicker {..}

arrangeModel
    :: PickedPath                    -- ^ Initial path.
    -> Event FilePath                -- ^ Programatic base directory changes.
    -> Event FilePath                -- ^ Programatic relative path changes.
    -> FilePathPicker                -- ^ Widget.
    -> UI (Behavior PickedPath)      -- ^ Behavior, capturing user
                                     -- and progranatic changes.
arrangeModel initial eProgBaseDir eProgRelPath widget = mdo

    let initialDir = baseDir initial
        initialRelPath = relativePath initial

        itxBaseDir = _itxBaseDir widget
        itxRelPath = _itxRelativePath widget

        -- Note these are only triggered on blur.
        eBaseDir = autocompleteBaseDirChange widget
        eRelPath = autocompleteRelativePathChange widget

        eBaseDirMoveUp = unsafeMapIO moveUpBasePath (bBaseDirModel <@ userMoveUp widget)
        eHome = unsafeMapIO (const getHomeDirectory) (userHome widget)

        eClearRelPath = "" <$ (userMoveUp widget `union` userHome widget)

    let eBaseDirModel = foldr union never
            [ eProgBaseDir
            , eHome
            , eBaseDirMoveUp
            , eBaseDir
            ]

        eRelPathModel = foldr union never
            [ eProgRelPath
            , eClearRelPath
            , eRelPath
            ]

    bBaseDirModel <- initialDir `stepper` eBaseDirModel
    bRelPathModel <- initialRelPath `stepper` eRelPathModel
    let bModel = PickedPath <$> bBaseDirModel <*> bRelPathModel

    let eUpdateDirCompletion = userBaseDirChange widget
            `union` (bBaseDirModel <@ UI.focus itxBaseDir)
        -- eExistingBaseDir exists because we only want to update the
        -- completion list if the directory exists. In particular, if
        -- the user is halfway through typing a directory name, the
        -- list should remain unchanged.
        eExistingBaseDir = filterJust (unsafeMapIO completionDir eUpdateDirCompletion)
        eDirListing = unsafeMapIO getDirListing eExistingBaseDir

        -- Updating the file completion list on focus is fine for our
        -- current purposes. If it ever becomes a possibility that the
        -- base directory changes while the relative path field is
        -- focused, we might also include userRelativePathChange, or
        -- introduce withRefresh mechanics like the one in the
        -- BoundedInput widget.
        eUpdateFileCompletion = bBaseDirModel <@ UI.focus itxRelPath
        eFileListing = unsafeMapIO getFileListing eUpdateFileCompletion

    initialDirExists <- liftIO $ doesDirectoryExist initialDir
    initialDirListing <- liftIO $ if initialDirExists
        then getDirListing initialDir
        else return []
    initialFileListing <- liftIO $ getFileListing initialDir

    bDirListing <- initialDirListing `stepper` eDirListing
    bFileListing <- initialFileListing `stepper` eFileListing

    -- Note the sinkWhen trick from the BoundedInput widget would foul
    -- up things here.
    element itxBaseDir
        # autocompleteInit
        # set autocompleteMinLength 0
        # sink autocompleteArraySource bDirListing
        # sink value (baseDir <$> bModel)

    onEvent (void eBaseDirMoveUp
            `union` void eHome
            `union` autocompleteselect itxBaseDir) $
        const (scrollToEnd itxBaseDir)

    askWindow >>= \w -> liftIOLater (runUI w (scrollToEnd itxBaseDir))

    element itxRelPath
        # autocompleteInit
        # set autocompleteMinLength 0
        # sink autocompleteArraySource bFileListing
        # sink value (relativePath <$> bModel)

    return bModel

userModel :: PickedPath -> FilePathPicker -> UI (Behavior PickedPath)
userModel initial = arrangeModel initial never never


moveUpBasePath :: FilePath -> IO FilePath
moveUpBasePath = fmap takeDirectory . expandSpecial
    where
    expandSpecial dir
        | null dir || all (== '.') dir = makeAbsolute dir
        | otherwise = return dir

blankDirToDot :: FilePath -> FilePath
blankDirToDot dir = if null dir then "." else dir

-- This implementation assumes we already checked that the directory
-- exists.
getDirListing :: FilePath -> IO [FilePath]
getDirListing dir = map removeLeadingDot <$> listDirectories dir
    where
    removeLeadingDot
        | dir == "." = makeRelative "."
        | otherwise = id

getFileListing :: FilePath -> IO [FilePath]
getFileListing dir = fmap (fmap takeFileName . filterTrkRpl) $ do
    let dir' = blankDirToDot dir
    exists <- doesDirectoryExist dir'
    if exists
        then listFiles dir'
        else return []
    where
    -- TODO: This should be customisable, presumably through new.
    filterTrkRpl :: [FilePath] -> [FilePath]
    filterTrkRpl = filter $ (\x -> x == ".TRK" || x == ".RPL")
        . takeExtension . map toUpper

completionDir :: FilePath -> IO (Maybe FilePath)
completionDir dir = do
    let dir' = blankDirToDot dir
    exists <- doesDirectoryExist dir'
    if exists
        then return $ Just dir'
        else do
            -- A trick so that completion does the right thing when
            -- using Shift + arrow key and then pressing a character.
            let dir'' = blankDirToDot $
                    (\xs -> guard (not $ null xs) >> init xs) dir'
            exists' <- doesDirectoryExist dir''
            return $ guard exists' >> Just dir''

scrollToEnd :: Element -> UI ()
scrollToEnd el = runFunction $ fun el
    where
    fun :: Element -> JSFunction ()
    fun = ffi "%1.scrollLeft = %1.scrollWidth"
