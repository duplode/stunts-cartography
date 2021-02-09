module Util.Threepenny.Flexbox
    ( rowFlex
    , subRowFlex
    , divVertFlex
    , overrideFlex
    , ParentProp(..)
    , ChildProp(..)
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Ext.Flexbox
import qualified Clay as Clay hiding (Clay.Flexbox)
import qualified Clay.Stylesheet as Clay (runS)
import qualified Clay.Flexbox as Flex

rowFlex :: UI Element
rowFlex = UI.div #. "row-flex"
    # setFlex parentProps
        { pFlexDirection = Clay.row
        , pFlexWrap = Flex.wrap
        , pJustifyContent = Clay.spaceBetween
        , pAlignItems = Clay.baseline
        }

-- | A sub-row differs from a row in that its contents are not
-- justified. In effect, we use it to group elements within a row.
--
-- N.B. The difference might be immaterial unless the sub-rows are
-- allowed to grow. In any case, it kind of makes sense to keep
-- track of it anyway.
subRowFlex :: UI Element
subRowFlex = rowFlex
    # overrideFlex [PJustifyContent Flex.flexStart] []

divVertFlex :: UI Element
divVertFlex = UI.div #. "div-side-flex"
    # setFlex parentProps
        { pFlexDirection = Clay.column
        }

-- | Overrides just the flex properties we want to change, as opposed
-- to setFlex and friends, which always set all properties.
overrideFlex :: [ParentProp] -> [ChildProp] -> UI Element -> UI Element
overrideFlex ps cs = set style ovs
   where
   ovs = concatMap toStyle ps ++ concatMap toStyle cs

-- | A sum type for parent flex properties.
data ParentProp
    = PDisplay Clay.Display
    | PFlexDirection Clay.FlexDirection
    | PFlexWrap Flex.FlexWrap
    | PJustifyContent Clay.JustifyContentValue
    | PAlignItems Clay.AlignItemsValue
    | PAlignContent Clay.AlignContentValue

instance ToStyle ParentProp where
    toStyle p = concatMap toStyle . Clay.runS $ case p of
        PDisplay x -> Clay.display x
        PFlexDirection x -> Flex.flexDirection x
        PFlexWrap x -> Flex.flexWrap x
        PJustifyContent x -> Flex.justifyContent x
        PAlignItems x -> Flex.alignItems x
        PAlignContent x -> Flex.alignContent x

-- | A sum type for child flex properties.
data ChildProp
    = COrder Int
    | CFlexGrow Int
    | CFlexShrink Int
    | CFlexBasis (Clay.Size Clay.LengthUnit)
    | CAlignSelf Clay.AlignSelfValue

instance ToStyle ChildProp where
    toStyle c = concatMap toStyle . Clay.runS $ case c of
        COrder x -> Flex.order x
        CFlexGrow x -> Flex.flexGrow x
        CFlexShrink x -> Flex.flexShrink x
        CFlexBasis x -> Flex.flexBasis x
        CAlignSelf x -> Flex.alignSelf x

