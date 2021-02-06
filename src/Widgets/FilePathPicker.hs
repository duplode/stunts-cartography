{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    -- Result type
    , PickedPath(..)
    ) where

import Data.List (sort)
import System.Directory
import System.FilePath
import Control.Monad
import Data.Char

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Util.Reactive.Threepenny
import Util.Threepenny.JQueryAutocomplete

data PickedPath = PickedPath
    { baseDir :: FilePath
    , relativePath :: FilePath
    }
    deriving (Eq, Ord, Show)

data FilePathPicker = FilePathPicker
    { _itxBaseDir :: Element
    , _itxRelativePath :: Element
    , _divWrapper :: Element

    , _userBaseDirChange :: Event FilePath
    , _userRelativePathChange :: Event FilePath
    , _autocompleteBaseDirChange :: Event FilePath
    , _autocompleteRelativePathChange :: Event FilePath
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

instance Widget FilePathPicker where
    getElement = _divWrapper

new :: UI FilePathPicker
new = do

    _itxBaseDir <-
        UI.input # set UI.type_ "text"
            #. "file-path-picker-base-path"

    _itxRelativePath <-
        UI.input # set UI.type_ "text"
            #. "file-path-picker-relative-path"

    let _userBaseDirChange = UI.valueChange _itxBaseDir
        _userRelativePathChange = UI.valueChange _itxRelativePath
        _autocompleteBaseDirChange = autocompleteValueChange _itxBaseDir
        _autocompleteRelativePathChange = autocompleteValueChange _itxRelativePath

    _strBasePathCaption <- string "Base path:"
        #. "file-path-picker-caption"

    _strRelativePathCaption <- string "TRK / RPL relative path:"
        #. "file-path-picker-caption"

    -- TODO: Make the captions customiseable.
    _divWrapper <- UI.div #. "file-path-picker" #+
        [ element _strBasePathCaption, UI.br
        , element _itxBaseDir, UI.br
        , element _strRelativePathCaption, UI.br
        , element _itxRelativePath
        ]

    return FilePathPicker {..}

arrangeModel
    :: PickedPath                    -- ^ Initial path.
    -> Event (FilePath -> FilePath)  -- ^ Programatic base directory changes.
    -> Event (FilePath -> FilePath)  -- ^ Programatic relative path changes.
    -> FilePathPicker                -- ^ Widget.
    -> UI (Behavior PickedPath)      -- ^ Behavior, capturing user
                                     -- and progranatic changes.
arrangeModel initial eProgBaseDir eProgRelPath widget = do

    let initialDir = baseDir initial
        initialRelPath = relativePath initial

    initialDirExists <- liftIO $ doesDirectoryExist initialDir
    initialDirListing <- liftIO $ if initialDirExists
        then getDirListing initialDir
        else return []
    initialFileListing <- liftIO $ getFileListing initialDir

    let
        -- Note these are only triggered on blur.
        eBaseDir = autocompleteBaseDirChange widget
        eRelPath = autocompleteRelativePathChange widget

        eModel = concatE
            [ (\f pick -> pick { baseDir = f (baseDir pick)}) <$> eProgBaseDir
            , (\f pick -> pick { relativePath = f (baseDir pick)}) <$> eProgRelPath
            , (\dir pick -> pick { baseDir = dir }) <$> eBaseDir
            , (\rel pick -> pick { relativePath = rel }) <$> eRelPath
            ]

    bModel <- initial `accumB` eModel

    let eKbBaseDir = userBaseDirChange widget
        eUpdateCompletion = union (baseDir <$> bModel <@ eProgBaseDir) eKbBaseDir
            `union` eBaseDir
        eExistingBaseDir = filterJust (unsafeMapIO completionDir eUpdateCompletion)
        eDirListing = unsafeMapIO getDirListing eExistingBaseDir
        eFileListing = unsafeMapIO getFileListing eUpdateCompletion

    bDirListing <- initialDirListing `stepper` eDirListing
    bFileListing <- initialFileListing `stepper` eFileListing

    -- Note the sinkWhen trick from the BoundedInput widget would foul
    -- up things here.
    element (_itxBaseDir widget)
        # autocompleteInit
        # sink autocompleteArraySource bDirListing
        # sink value (baseDir <$> bModel)

    element (_itxRelativePath widget)
        # autocompleteInit
        # sink autocompleteArraySource bFileListing
        # sink value (relativePath <$> bModel)

    return bModel

userModel :: PickedPath -> FilePathPicker -> UI (Behavior PickedPath)
userModel initial = arrangeModel initial never never


-- Auxiliary definitions for the autocompletion setup in arrangeModel.

getDirListing :: FilePath -> IO [FilePath]
getDirListing dir = fmap sort $ do
    -- Assuming we already checked that dir exists.
    getDirectoryContents dir
        >>= (filterM doesDirectoryExist
            . map ((dotToBlankDir dir) </>))

blankDirToDot :: FilePath -> FilePath
blankDirToDot dir = if null dir then "." else dir

dotToBlankDir :: FilePath -> FilePath
dotToBlankDir dir = case dir of
    "." -> ""
    _ -> dir

toFileListing :: Event FilePath -> Event [FilePath]
toFileListing = unsafeMapIO getFileListing

getFileListing :: FilePath -> IO [FilePath]
getFileListing dir = fmap (sort . filterTrkRpl) $ do
    let dir' = blankDirToDot dir
    exists <- doesDirectoryExist dir'
    if exists
        then getDirectoryContents dir'
                >>= (filterM $ doesFileExist . (dir' </>))
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
