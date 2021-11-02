module Util.Threepenny.Flexbox
    ( rowFlex
    , subRowFlex
    , divVertFlex
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Ext.Flexbox
import qualified Clay as Clay hiding (Clay.Flexbox)
--import qualified Clay.Stylesheet as Clay (runS)
import qualified Clay.Flexbox as Flex

rowFlex :: UI Element
rowFlex = UI.div #. "row-flex"
    # setFlex
        ( flexDirection Clay.row
        <> flexWrap Flex.wrap
        <> justifyContent Clay.spaceBetween
        <> alignItems Clay.baseline
        )

-- | A sub-row differs from a row in that its contents are not
-- justified. In effect, we use it to group elements within a row.
--
-- N.B. The difference might be immaterial unless the sub-rows are
-- allowed to grow. In any case, it kind of makes sense to keep
-- track of it anyway.
subRowFlex :: UI Element
subRowFlex = rowFlex
    # modifyFlex (justifyContent Flex.flexStart)

divVertFlex :: UI Element
divVertFlex = UI.div #. "div-side-flex"
    # setFlex (flexDirection Clay.column)
