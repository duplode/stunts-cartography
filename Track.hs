module Track
    ( Orientation(..)
    , Chirality(..)
    , rotateOrientation
    , Element(..)
    , ElementType(..)
    , ElementSurface(..)
    , ElementAttribute(..)
    , ElementSize(..)
    , TerrainType(..)
    , Horizon(..)
    , Tile(..)
    , getTileOrientation
    , getTileChirality
    , getTileSize
    , getAbstractTileSize
    , getTileSurface
    , getTerrainOrientation
    , getElementType
    , getTerrainType
    , isElemAttrOf

    , blankTile

    , veryRawReadTrack
    , rawTrackToTileArray
    , horizonFromRawTrack

    , terrainTrkSimple
    ) where

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import qualified OurByteString as LB
import Data.Word (Word8)
import Data.Array
import Utils

-- |The .TRK byte string with its components separated and the padding byte
-- excised.
--
data VeryRawTrack = VeryRawTrack
    { veryRawElements :: LB.ByteString -- ^ Element values as raw byte string.
    , veryRawHorizon  :: Word8         -- ^ Horizon byte.
    , veryRawTerrain  :: LB.ByteString -- ^ Terrain values as raw byte string.
    }

-- |Minimal processing of a .TRK byte string; the components are separated and
-- nothing else is done. The argument byte string must have been validated
-- previously.
--
veryRawReadTrack :: LB.ByteString -> VeryRawTrack
veryRawReadTrack dat = VeryRawTrack elms scen terr
    where
    (elms, (scen, terr)) = fmap (LB.take 900) . fromJust . LB.uncons
        <$> LB.splitAt 900 dat

-- |For extra simplicity, we use just one simple datatype to represent
-- orientation and direction. The values 'Q1'..'Q4' stand for the four
-- trigonometric quadrants. The relevant conventions are:
--
-- * For most linear elements, 'Q1' means that the track runs in the direction
--   of the angle 0; Q2 implies a pi/2 direction, and so forth.
--
-- * Ramps and u/d corks are considered to face their ascending direction.
--
-- * Entrance/exit tiles (pipes, highways, s/f lines) are considered to face
--   their entrance direction.
--
-- * The major exception are straight banked roads and banked road transitions,
--   whose orientation is given by the ascending direction of the banking.
--
-- * For corners, 'Q1'..'Q4' indicate the quadrant spanned by the arc of the
--   corner.
--
-- * The orientation of a split is that of the straight path when the split
--   is driven as a path divide, and not as a rejoinder.
--
-- * The orientation of a scenery element is given by the direction the front
--   side of the building/vehicle/etc. faces.
--
-- * The orientation of filler tiles is ignored.
--
-- * Straight hill slopes recieve orientations as if they were ramps. Angled
--   terrain pieces recieve orientations as if they were small corners
--   centered in the tile vertex which is fully inside water or atop the hill.
--
data Orientation = Q1 -- ^ First quadrant, [0, pi/2[
                 | Q2 -- ^ Second quadrant, [pi/2, pi[
                 | Q3 -- ^ Third quadrant, [pi, 3*pi/2[
                 | Q4 -- ^ Fourth quadrant, [3*pi/2, 2*pi[
                 deriving (Eq, Ord, Show)

-- |The Enum instance for 'Orientation' is defined so that 'succ' corresponds
-- to anticlockwise rotation by pi/2.
--
instance Enum Orientation where
    toEnum 0    = Q1
    toEnum 1    = Q2
    toEnum 2    = Q3
    toEnum 3    = Q4
    toEnum x    = toEnum (x `mod` 4)
    fromEnum Q1 = 0
    fromEnum Q2 = 1
    fromEnum Q3 = 2
    fromEnum Q4 = 3

-- |Applies an arbitrary number of quarter turns to an 'Orientation'.
rotateOrientation :: Int -- ^ Number of quarter-turns. Positive values rotate
                         -- anticlockwise; negative ones, clockwise.
                  -> Orientation
                  -> Orientation
rotateOrientation turns = toEnum . (+ turns) . fromEnum

-- |Chirality of a track element. 'Dextral' means clockwise; 'Sinistral',
-- anticlockwise and 'Achiral' that it doesn't matter. To what "clockwise" and
-- "anticlockwise" refer depends on the track element being considered:
--
-- * U/d corks: clockwiseness of the rotation when ascending the cork.
--
-- * Chicanes: clockwiseness when tracing an arc from the axis of the direction
--   of the car when entering the chicane to the exit point of the chicane. In a
--   possibly less confusing way: a dextral/clockwise chicane can be rotated so
--   that it looks like a plot of the tangent function (as opposed to a plot of
--   the cotangent).
--
-- * Dual-way splits: clockwiseness of the corner when driven as a path divide
--   (as opposed to a path rejoinder).
--
-- * Banked road transitions: clockwiseness of the only banked corner that fits
--   the transition when driven as a banked road entrance.
--
-- Some of the chiral elements in Stunts unfortunately lack one of the chiral
-- counterparts, and so they are treated as 'Achiral'. Were it not the case,
-- their clockwiseness would be determined in the following way:
--
-- * L/r corks: clockwiseness around the track direction axis (so that the
--   corks l/r in Stunts are sinistral/anticlockwise).
--
-- * Loopings: clockwiseness around the left-to-right axis as seen when
--   apporaching the loop (so that Stunts loops are dextral/clockwise).
--
-- * Slalom blocks: clockwiseness of a chicane placed just before the blocks
--   so that the second apex of the chicane is on the opposite side of the
--   track in relation to the first slalom block (so that Stunts slalom blocks
--   are dextral/clockwise).
--
data Chirality = Achiral
               | Dextral
               | Sinistral
               deriving (Eq, Enum, Show, Ord)

-- |Inverts a chirality value.
--
flipChirality :: Chirality -> Chirality
flipChirality Dextral   = Sinistral
flipChirality Sinistral = Dextral
flipChirality c         = c

-- |Connectivity of a track element. Together with the chirality, it determines
-- at which tile sides the element connects to others in a track path. It also
-- sets the tile shape of the element, in conjunction to the element length.
-- Terrain elements also have connectivities, defined in an analogous way to
-- those of track elements.
--
data Connectivity = Isolate      -- ^ Blank tiles and scenery. n x n shape.
                  | Linear       -- ^ Straight roads, loops, etc. n x 1 shape.
                  | Orthogonal   -- ^ Banked roads and transitions. n x 1.
                  | Corner       -- ^ Corners, small and large. n x n.
                  | Oblique      -- ^ Chicanes. n x n. Must be chiral.
                  | ChiralLinear -- ^ U/d corks. n x n. Must be chiral.
                  | Split        -- ^ Path divide. n x n. Must be chiral.
                  | Crossing     -- ^ Crossroads. 1 x 1 (length ignored).
                  deriving (Eq, Enum, Show)

-- |Surface types of track elements.
--
data ElementSurface = Tarmac
                    | Dirt
                    | Ice
                    deriving (Eq, Enum, Show, Ord)

-- | Abstract size of a track element.
data ElementSize = Small -- ^ 1 x 1
                 | Large -- ^ 2 x 1 or 2 x 2, depending on connectivity.
                 deriving (Eq, Ord, Enum, Show)

-- |Types of track element, without reference to orientation, chirality,
-- connectivity or surface.
--
data ElementType = Blank
                 | StartFinish
                 | Road
                 | Crossroad
                 | ElevatedRoad
                 | SolidRoad
                 | SpanOverRoad
                 | ElevatedSpan
                 | ElevatedRamp
                 | BridgeRamp
                 | SolidRamp
                 | CorkUpDown
                 | Looping
                 | CorkLeftRight
                 | Tunnel
                 | SlalomRoad
                 | Pipe
                 | PipeObstacle
                 | PipeTransition
                 | Highway
                 | HighwayTransition
                 | BankedRoad
                 | SharpCorner
                 | LargeCorner
                 | BankedCorner
                 | ElevatedCorner
                 | SharpSplit
                 | LargeSplit
                 | Chicane
                 | BankedTransition
                 | PlayerGhost
                 | OpponentGhost
                 | Palm
                 | Cactus
                 | Pine
                 | TennisCourt
                 | GasStation
                 | Barn
                 | OfficeBuilding
                 | Windmill
                 | Ship
                 | Joe's
                 | FillerQ1
                 | FillerQ3
                 | FillerQ4
                 | UnknownElement
                 deriving (Eq, Enum, Show, Ord)

-- |Types of terrain element, without reference to orientation, chirality,
-- connectivity or surface.
data TerrainType = Plain
                 | Water
                 | AngledMargin
                 | Hill
                 | Slope
                 | OuterAngledSlope
                 | InnerAngledSlope
                 | UnknownTerrain
                 deriving (Eq, Enum, Show, Ord)

-- |A track element, with the necessary data to reconstruct all its traits.
data Element = Element
    { elementType        :: ElementType
    , elementSurface     :: ElementSurface
    , elementChirality   :: Chirality
    , elementOrientation :: Orientation
    }
    deriving (Eq, Show, Ord)

-- |Extra metadata about elements.
data ElementAttribute = Elevated
                      | Solid
                      | Ramp
                      | Banked -- ^ Does not apply to the transitions.
                      | Transition
                      | Chiral
                      | FakeGrass
                      | Scenery
                      | Filler
                      deriving (Eq, Enum, Show)

-- TODO: Decide how to describe connectivity restrictions per type.

-- |Element properties which depend on the 'ElementType' alone.
data ElementProperties = ElementProperties
    { elementConnectivity :: Connectivity
    , elementAttributes   :: [ElementAttribute]
    , elementSize         :: ElementSize
    }
    deriving (Eq, Show)

-- Various property builders
blankProperties :: ElementProperties
blankProperties = ElementProperties Isolate [] Small

smallLinearProperties :: ElementProperties
smallLinearProperties = ElementProperties Linear [] Small

largeLinearProperties :: ElementProperties
largeLinearProperties = ElementProperties Linear [] Large

largeCornerProperties :: ElementProperties
largeCornerProperties = ElementProperties Corner [] Large

addAttribute :: ElementAttribute -> ElementProperties -> ElementProperties
addAttribute extra props =
    props {elementAttributes = extra : elementAttributes props}

elevatedProperties :: ElementProperties
elevatedProperties = Elevated `addAttribute` smallLinearProperties

transitionProperties :: ElementProperties
transitionProperties = Transition `addAttribute` smallLinearProperties

rampProperties :: ElementProperties
rampProperties = Ramp `addAttribute` transitionProperties

sceneryProperties :: ElementProperties
sceneryProperties = Scenery `addAttribute` blankProperties

fillerProperties :: ElementProperties
fillerProperties = Filler `addAttribute` blankProperties

-- |Recovers the element properties of a type.
eTypeToProps :: ElementType -> ElementProperties
eTypeToProps et
    | test Blank = blankProperties
    | testMany roads = smallLinearProperties
    | test Crossroad = ElementProperties Crossing [] Small
    | testMany elevs = elevatedProperties
    | test SolidRoad = Solid `addAttribute` elevatedProperties
    | test SpanOverRoad = ElementProperties Crossing [Elevated] Small
    | testMany ramps = rampProperties
    | test SolidRamp = Solid `addAttribute` rampProperties
    | test CorkUpDown = ElementProperties ChiralLinear [Chiral] Large
    | testMany lLins = largeLinearProperties
    | testMany trans = transitionProperties
    | test BankedRoad = ElementProperties Orthogonal [] Small
    | test SharpCorner = ElementProperties Corner [] Small
    | test LargeCorner = largeCornerProperties
    | test BankedCorner = Banked `addAttribute` largeCornerProperties
    | test ElevatedCorner = Elevated `addAttribute` largeCornerProperties
    | test SharpSplit = ElementProperties Split [Chiral] Small
    | test LargeSplit = ElementProperties Split [Chiral] Large
    | test Chicane = ElementProperties Oblique [Chiral, FakeGrass] Large
    | test BankedTransition = ElementProperties Orthogonal
        [Chiral, Transition] Small
    | testMany fillers = fillerProperties
    | otherwise = sceneryProperties -- TODO: consider UnknownElement.
    where
    test = (et ==)
    testMany = or . map test
    roads = [ Road
            , StartFinish
            , Tunnel
            , SlalomRoad
            , Pipe
            , PipeObstacle
            , Highway
            ]
    elevs = [ ElevatedRoad
            , ElevatedSpan
            ]
    ramps = [ ElevatedRamp
            , BridgeRamp
            ]
    lLins = [ Looping
            , CorkLeftRight
            ]
    trans = [ PipeTransition
            , HighwayTransition
            ]
    fillers = [ FillerQ1
              , FillerQ3
              , FillerQ4
              ]

-- |Tests an element type for an attribute.
isElemAttrOf :: ElementAttribute -> ElementType -> Bool
attr `isElemAttrOf` et =
    attr `elem` (elementAttributes . eTypeToProps $ et)

-- |Recovers the size of an element, in tile units.
getElementSize :: Element
               -> (Int, Int) -- ^ Pair of lengths. The first value is the
                             -- length along the entry direction of the
                             -- element; the second, the length across it.
getElementSize el
    | conn == Linear || conn == Orthogonal = (sz, 1)
    | conn == Crossing                     = (1, 1)
    | otherwise                            = (sz, sz)
    where
    ElementProperties conn _ elSize = eTypeToProps . elementType $ el
    sz = case elSize of
        Large -> 2
        _     -> 1

-- |Recovers the size specification of an element.
getAbstractSize :: Element -> ElementSize
getAbstractSize = elementSize . eTypeToProps . elementType

-- Smart constructors. TODO: These will be exported, document.
-- TODO: consider const'ing or removing the argument from the rotationally
-- invariant elements.

blank :: Orientation -> Element
blank = Element Blank Tarmac Achiral

pavedStartFinish :: Orientation -> Element
pavedStartFinish = Element StartFinish Tarmac Achiral

dirtStartFinish :: Orientation -> Element
dirtStartFinish = Element StartFinish Dirt Achiral

icyStartFinish :: Orientation -> Element
icyStartFinish = Element StartFinish Ice Achiral

pavedRoad :: Orientation -> Element
pavedRoad = Element Road Tarmac Achiral

dirtRoad :: Orientation -> Element
dirtRoad = Element Road Dirt Achiral

icyRoad :: Orientation -> Element
icyRoad = Element Road Ice Achiral

pavedCrossroad :: Orientation -> Element
pavedCrossroad = Element Crossroad Tarmac Achiral

dirtCrossroad :: Orientation -> Element
dirtCrossroad = Element Crossroad Dirt Achiral

icyCrossroad :: Orientation -> Element
icyCrossroad = Element Crossroad Ice Achiral

elevatedRoad :: Orientation -> Element
elevatedRoad = Element ElevatedRoad Tarmac Achiral

solidRoad :: Orientation -> Element
solidRoad = Element SolidRoad Tarmac Achiral

spanOverRoad :: Orientation -> Element
spanOverRoad = Element SpanOverRoad Tarmac Achiral

elevatedSpan :: Orientation -> Element
elevatedSpan = Element ElevatedSpan Tarmac Achiral

elevatedRamp :: Orientation -> Element
elevatedRamp = Element ElevatedRamp Tarmac Achiral

bridgeRamp :: Orientation -> Element
bridgeRamp = Element BridgeRamp Tarmac Achiral

solidRamp :: Orientation -> Element
solidRamp = Element SolidRamp Tarmac Achiral

corkUpDownDex :: Orientation -> Element
corkUpDownDex = Element CorkUpDown Tarmac Dextral

corkUpDownSin :: Orientation -> Element
corkUpDownSin = Element CorkUpDown Tarmac Sinistral

looping :: Orientation -> Element
looping = Element Looping Tarmac Achiral

corkLeftRight :: Orientation -> Element
corkLeftRight = Element CorkLeftRight Tarmac Achiral

tunnel :: Orientation -> Element
tunnel = Element Tunnel Tarmac Achiral

slalomRoad :: Orientation -> Element
slalomRoad = Element SlalomRoad Tarmac Achiral

pipe :: Orientation -> Element
pipe = Element Pipe Tarmac Achiral

pipeObstacle :: Orientation -> Element
pipeObstacle = Element PipeObstacle Tarmac Achiral

pipeTransition :: Orientation -> Element
pipeTransition = Element PipeTransition Tarmac Achiral

highway :: Orientation -> Element
highway = Element Highway Tarmac Achiral

highwayTransition :: Orientation -> Element
highwayTransition = Element HighwayTransition Tarmac Achiral

bankedRoad :: Orientation -> Element
bankedRoad = Element BankedRoad Tarmac Achiral

pavedSharpCorner :: Orientation -> Element
pavedSharpCorner = Element SharpCorner Tarmac Achiral

dirtSharpCorner :: Orientation -> Element
dirtSharpCorner = Element SharpCorner Dirt Achiral

icySharpCorner :: Orientation -> Element
icySharpCorner = Element SharpCorner Ice Achiral

pavedLargeCorner :: Orientation -> Element
pavedLargeCorner = Element LargeCorner Tarmac Achiral

dirtLargeCorner :: Orientation -> Element
dirtLargeCorner = Element LargeCorner Dirt Achiral

icyLargeCorner :: Orientation -> Element
icyLargeCorner = Element LargeCorner Ice Achiral

bankedCorner :: Orientation -> Element
bankedCorner = Element BankedCorner Tarmac Achiral

elevatedCorner :: Orientation -> Element
elevatedCorner = Element ElevatedCorner Tarmac Achiral

pavedSharpSplitDex :: Orientation -> Element
pavedSharpSplitDex = Element SharpSplit Tarmac Dextral

dirtSharpSplitDex :: Orientation -> Element
dirtSharpSplitDex = Element SharpSplit Dirt Dextral

icySharpSplitDex :: Orientation -> Element
icySharpSplitDex = Element SharpSplit Ice Dextral

pavedSharpSplitSin :: Orientation -> Element
pavedSharpSplitSin = Element SharpSplit Tarmac Sinistral

dirtSharpSplitSin :: Orientation -> Element
dirtSharpSplitSin = Element SharpSplit Dirt Sinistral

icySharpSplitSin :: Orientation -> Element
icySharpSplitSin = Element SharpSplit Ice Sinistral

largeSplitDex :: Orientation -> Element
largeSplitDex = Element LargeSplit Tarmac Dextral

largeSplitSin :: Orientation -> Element
largeSplitSin = Element LargeSplit Tarmac Sinistral

chicaneDex :: Orientation -> Element
chicaneDex = Element Chicane Tarmac Dextral

chicaneSin :: Orientation -> Element
chicaneSin = Element Chicane Tarmac Sinistral

bankedTransitionDex :: Orientation -> Element
bankedTransitionDex = Element BankedTransition Tarmac Dextral

bankedTransitionSin :: Orientation -> Element
bankedTransitionSin = Element BankedTransition Tarmac Sinistral

playerGhost :: Orientation -> Element
playerGhost = Element PlayerGhost Tarmac Achiral

opponentGhost :: Orientation -> Element
opponentGhost = Element OpponentGhost Tarmac Achiral

palm :: Orientation -> Element
palm = Element Palm Tarmac Achiral

cactus :: Orientation -> Element
cactus = Element Cactus Tarmac Achiral

pine :: Orientation -> Element
pine = Element Pine Tarmac Achiral

tennisCourt :: Orientation -> Element
tennisCourt = Element TennisCourt Tarmac Achiral

gasStation :: Orientation -> Element
gasStation = Element GasStation Tarmac Achiral

barn :: Orientation -> Element
barn = Element Barn Tarmac Achiral

officeBuilding :: Orientation -> Element
officeBuilding = Element OfficeBuilding Tarmac Achiral

windmill :: Orientation -> Element
windmill = Element Windmill Tarmac Achiral

ship :: Orientation -> Element
ship = Element Ship Tarmac Achiral

joe's :: Orientation -> Element
joe's = Element Joe's Tarmac Achiral

fillerQ1 :: Orientation -> Element
fillerQ1 = Element FillerQ1 Tarmac Achiral

fillerQ3 :: Orientation -> Element
fillerQ3 = Element FillerQ3 Tarmac Achiral

fillerQ4 :: Orientation -> Element
fillerQ4 = Element FillerQ4 Tarmac Achiral

unknownElement :: Orientation -> Element
unknownElement = Element UnknownElement Tarmac Achiral

-- |Builds an element from the .TRK byte value.
byteToElement :: Word8 -> Element
byteToElement x = case x of
    0x00 -> blank Q1
    0x01 -> pavedStartFinish Q2
    0x02 -> playerGhost Q2
    0x03 -> opponentGhost Q2
    0x04 -> pavedRoad Q2
    0x05 -> pavedRoad Q1
    0x06 -> pavedSharpCorner Q2
    0x07 -> pavedSharpCorner Q1
    0x08 -> pavedSharpCorner Q3
    0x09 -> pavedSharpCorner Q4
    0x0A -> pavedLargeCorner Q2
    0x0B -> pavedLargeCorner Q1
    0x0C -> pavedLargeCorner Q3
    0x0D -> pavedLargeCorner Q4
    0x0E -> dirtRoad Q2
    0x0F -> dirtRoad Q1
    0x10 -> dirtSharpCorner Q2
    0x11 -> dirtSharpCorner Q1
    0x12 -> dirtSharpCorner Q3
    0x13 -> dirtSharpCorner Q4
    0x14 -> dirtLargeCorner Q2
    0x15 -> dirtLargeCorner Q1
    0x16 -> dirtLargeCorner Q3
    0x17 -> dirtLargeCorner Q4
    0x18 -> icyRoad Q2
    0x19 -> icyRoad Q1
    0x1A -> icySharpCorner Q2
    0x1B -> icySharpCorner Q1
    0x1C -> icySharpCorner Q3
    0x1D -> icySharpCorner Q4
    0x1E -> icyLargeCorner Q2
    0x1F -> icyLargeCorner Q1
    0x20 -> icyLargeCorner Q3
    0x21 -> icyLargeCorner Q4
    0x22 -> elevatedRoad Q2
    0x23 -> elevatedRoad Q1
    0x24 -> elevatedRamp Q1
    0x25 -> elevatedRamp Q3
    0x26 -> elevatedRamp Q2
    0x27 -> elevatedRamp Q4
    0x28 -> bankedTransitionDex Q3
    0x29 -> bankedTransitionDex Q2
    0x2A -> bankedTransitionDex Q1
    0x2B -> bankedTransitionDex Q4
    0x2C -> bankedTransitionSin Q1
    0x2D -> bankedTransitionSin Q4
    0x2E -> bankedTransitionSin Q3
    0x2F -> bankedTransitionSin Q2
    0x30 -> bankedRoad Q3
    0x31 -> bankedRoad Q1
    0x32 -> bankedRoad Q2
    0x33 -> bankedRoad Q4
    0x34 -> bankedCorner Q2
    0x35 -> bankedCorner Q1
    0x36 -> bankedCorner Q3
    0x37 -> bankedCorner Q4
    0x38 -> bridgeRamp Q1
    0x39 -> bridgeRamp Q3
    0x3A -> bridgeRamp Q2
    0x3B -> bridgeRamp Q4
    0x3C -> chicaneDex Q2
    0x3D -> chicaneSin Q1
    0x3E -> chicaneSin Q2
    0x3F -> chicaneDex Q1
    0x40 -> looping Q2
    0x41 -> looping Q1
    0x42 -> tunnel Q2
    0x43 -> tunnel Q1
    0x44 -> pipe Q2
    0x45 -> pipe Q1
    0x46 -> pipeTransition Q2
    0x47 -> pipeTransition Q4
    0x48 -> pipeTransition Q3
    0x49 -> pipeTransition Q1
    0x4A -> pavedCrossroad Q1
    0x4B -> pavedSharpSplitSin Q2
    0x4C -> pavedSharpSplitSin Q3
    0x4D -> pavedSharpSplitSin Q4
    0x4E -> pavedSharpSplitSin Q1
    0x4F -> pavedSharpSplitDex Q2
    0x50 -> pavedSharpSplitDex Q3
    0x51 -> pavedSharpSplitDex Q4
    0x52 -> pavedSharpSplitDex Q1
    0x53 -> pipeObstacle Q2
    0x54 -> pipeObstacle Q1
    0x55 -> corkLeftRight Q2
    0x56 -> corkLeftRight Q1
    0x57 -> largeSplitSin Q2
    0x58 -> largeSplitSin Q3
    0x59 -> largeSplitSin Q4
    0x5A -> largeSplitSin Q1
    0x5B -> largeSplitDex Q2
    0x5C -> largeSplitDex Q3
    0x5D -> largeSplitDex Q4
    0x5E -> largeSplitDex Q1
    0x5F -> solidRamp Q1
    0x60 -> solidRamp Q3
    0x61 -> solidRamp Q2
    0x62 -> solidRamp Q4
    0x63 -> solidRoad Q2
    0x64 -> solidRoad Q1
    0x65 -> spanOverRoad Q2
    0x66 -> spanOverRoad Q1
    0x67 -> elevatedSpan Q2
    0x68 -> elevatedSpan Q1
    0x69 -> elevatedCorner Q2
    0x6A -> elevatedCorner Q1
    0x6B -> elevatedCorner Q3
    0x6C -> elevatedCorner Q4
    0x6D -> highway Q2
    0x6E -> highway Q1
    0x6F -> highwayTransition Q2
    0x70 -> highwayTransition Q1
    0x71 -> highwayTransition Q4
    0x72 -> highwayTransition Q3
    0x73 -> slalomRoad Q2
    0x74 -> slalomRoad Q1
    0x75 -> corkUpDownDex Q2
    0x76 -> corkUpDownDex Q3
    0x77 -> corkUpDownDex Q4
    0x78 -> corkUpDownDex Q1
    0x79 -> corkUpDownSin Q2
    0x7A -> corkUpDownSin Q3
    0x7B -> corkUpDownSin Q4
    0x7C -> corkUpDownSin Q1
    0x7D -> dirtCrossroad Q1
    0x7E -> dirtSharpSplitSin Q2
    0x7F -> dirtSharpSplitSin Q3
    0x80 -> dirtSharpSplitSin Q4
    0x81 -> dirtSharpSplitSin Q1
    0x82 -> dirtSharpSplitDex Q2
    0x83 -> dirtSharpSplitDex Q3
    0x84 -> dirtSharpSplitDex Q4
    0x85 -> dirtSharpSplitDex Q1
    0x86 -> dirtStartFinish Q2
    0x87 -> dirtStartFinish Q4
    0x88 -> dirtStartFinish Q1
    0x89 -> dirtStartFinish Q3
    0x8A -> icyCrossroad Q1
    0x8B -> icySharpSplitSin Q2
    0x8C -> icySharpSplitSin Q3
    0x8D -> icySharpSplitSin Q4
    0x8E -> icySharpSplitSin Q1
    0x8F -> icySharpSplitDex Q2
    0x90 -> icySharpSplitDex Q3
    0x91 -> icySharpSplitDex Q4
    0x92 -> icySharpSplitDex Q1
    0x93 -> icyStartFinish Q2
    0x94 -> icyStartFinish Q4
    0x95 -> icyStartFinish Q1
    0x96 -> icyStartFinish Q3
    0x97 -> palm Q1
    0x98 -> cactus Q1
    0x99 -> pine Q1
    0x9A -> tennisCourt Q1
    0x9B -> gasStation Q4
    0x9C -> gasStation Q2
    0x9D -> gasStation Q1
    0x9E -> gasStation Q3
    0x9F -> barn Q4
    0xA0 -> barn Q2
    0xA1 -> barn Q1
    0xA2 -> barn Q3
    0xA3 -> officeBuilding Q4
    0xA4 -> officeBuilding Q2
    0xA5 -> officeBuilding Q1
    0xA6 -> officeBuilding Q3
    0xA7 -> windmill Q4
    0xA8 -> windmill Q2
    0xA9 -> windmill Q1
    0xAA -> windmill Q3
    0xAB -> ship Q3
    0xAC -> ship Q1
    0xAD -> ship Q4
    0xAE -> ship Q2
    0xAF -> joe's Q4
    0xB0 -> joe's Q2
    0xB1 -> joe's Q1
    0xB2 -> joe's Q3
    0xB3 -> pavedStartFinish Q4
    0xB4 -> pavedStartFinish Q1
    0xB5 -> pavedStartFinish Q3
    0xFD -> fillerQ4 Q1
    0xFE -> fillerQ3 Q1
    0xFF -> fillerQ1 Q1
    _    -> unknownElement Q1


-- |A piece of terrain, with the data needed to reconstruct all its traits.
data Terrain = Terrain
    { terrainType        :: TerrainType
    , terrainOrientation :: Orientation
    }
    deriving (Eq, Show)

-- TODO: Incorporate terrain connectivity.

-- Terrain smart constructors.

plain :: Orientation -> Terrain
plain = Terrain Plain

water :: Orientation -> Terrain
water = Terrain Water

angledMargin :: Orientation -> Terrain
angledMargin = Terrain AngledMargin

hill :: Orientation -> Terrain
hill = Terrain Hill

slope :: Orientation -> Terrain
slope = Terrain Slope

outerAngledSlope :: Orientation -> Terrain
outerAngledSlope = Terrain OuterAngledSlope

innerAngledSlope :: Orientation -> Terrain
innerAngledSlope = Terrain InnerAngledSlope

unknownTerrain :: Orientation -> Terrain
unknownTerrain = Terrain UnknownTerrain


-- |Builds a terrain piece from the .TRK byte value.
byteToTerrain :: Word8 -> Terrain
byteToTerrain x = case x of
    0x00 -> plain Q1
    0x01 -> water Q1
    0x02 -> angledMargin Q1
    0x03 -> angledMargin Q2
    0x04 -> angledMargin Q3
    0x05 -> angledMargin Q4
    0x06 -> hill Q1
    0x07 -> slope Q2
    0x08 -> slope Q3
    0x09 -> slope Q4
    0x0A -> slope Q1
    0x0B -> outerAngledSlope Q4
    0x0C -> outerAngledSlope Q1
    0x0D -> outerAngledSlope Q2
    0x0E -> outerAngledSlope Q3
    0x0F -> innerAngledSlope Q4
    0x10 -> innerAngledSlope Q1
    0x11 -> innerAngledSlope Q2
    0x12 -> innerAngledSlope Q3
    _    -> unknownTerrain Q1

-- |A tile is a conjunction of a track element and a terrain piece.
data Tile = Tile
    { tileElement :: Element
    , tileTerrain :: Terrain
    }
    deriving (Eq, Show)

-- |An empty tile.
blankTile :: Tile
blankTile = Tile (blank Q1) (plain Q1)

-- |Gets the underlying element type of a tile.
getElementType :: Tile -> ElementType
getElementType = elementType . tileElement

-- |Gets the size of the tile element. TODO: kind of a misnomer.
getTileSize :: Tile
            -> (Int, Int) -- ^ See 'getElementSize' for semantics.
getTileSize = getElementSize . tileElement

-- |Gets the size specification of the tile element.
getAbstractTileSize :: Tile -> ElementSize
getAbstractTileSize = getAbstractSize . tileElement

-- |Gets the orientation of a tile element.
getTileOrientation :: Tile -> Orientation
getTileOrientation = elementOrientation . tileElement

-- |Gets the chirality of a tile element.
getTileChirality :: Tile -> Chirality
getTileChirality = elementChirality . tileElement

-- |Gets the surface type of a tile element.
getTileSurface :: Tile -> ElementSurface
getTileSurface = elementSurface . tileElement

-- |Gets the underlying terrain type of a tile.
getTerrainType :: Tile -> TerrainType
getTerrainType = terrainType . tileTerrain

-- |Gets the orientation of a tile terrain.
getTerrainOrientation :: Tile -> Orientation
getTerrainOrientation = terrainOrientation . tileTerrain

-- |Abstract representation of the track horizon
data Horizon = Desert
             | Tropical
             | Alpine
             | City
             | Country
             | UnknownHorizon
             deriving (Eq, Enum, Show)

-- |Obtains the horizon which corresponds to a byte value
byteToHorizon :: Word8 -> Horizon
byteToHorizon x
    | x < 5     = toEnum $ fromIntegral x
    | otherwise = UnknownHorizon

-- |Converts a raw track into an array. Indexing is done in the obvious way
-- (Cartesian coordinates, origin at bottom left, zero-based).
rawTrackToTileArray :: VeryRawTrack -> Array (Int, Int) Tile
rawTrackToTileArray trk = listArray ((0, 0), (29, 29)) tiles
    where
    elmVals = veryRawElements trk
    terVals = LB.concat . reverse
        . bsSplitAtEvery30th $ veryRawTerrain trk
    tiles = zipWith Tile (byteToElement <$> LB.unpack elmVals)
        (byteToTerrain <$> LB.unpack terVals)
    --indices = [ (x, y) | x <- [0..29], y <- [0..29]]

        -- . splitAtEvery30th $ veryRawTerrain trk
    {-
    terVals = LB.concat . reverse . map fst
        . takeWhile (not . LB.null . fst) . drop 1
        . iterate (LB.splitAt 30 . snd) $ (LB.empty, veryRawTerrain trk)
        -}

-- |Retrieves the horizon of a raw track.
horizonFromRawTrack :: VeryRawTrack -> Horizon
horizonFromRawTrack = byteToHorizon . veryRawHorizon


-- |Extracts the terrain part of the unprocessed TRK bytestring.
-- No sanity checks.
terrainTrkSimple :: LB.ByteString -> LB.ByteString
terrainTrkSimple = LB.append (LB.pack $ replicate 0x384 0) . LB.drop 0x384

