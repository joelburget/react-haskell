module React.Style where

import qualified Data.Aeson as Aeson
import Data.Colour
import qualified Data.HashMap.Strict as H
import Data.Text (Text)

import React.Types

-- Invariant:
newtype Style = Style { unStyle :: H.HashMap Text Aeson.Object }

instance Monoid Style where
    mempty = H.empty
    mappend h1 h2 = h1 <> h2

style_ :: [ Style ] -> AttrOrHandler sig
style_ = StaticAttr "style" . Aeson.Object . unStyle . mconcat

data DirectionType = Cardinal | Ordinal

data Direction :: DirectionType -> * where
    Left        :: Direction Cardinal
    Right       :: Direction Cardinal
    Top         :: Direction Cardinal
    Bottom      :: Direction Cardinal

    TopLeft     :: Direction Ordinal
    TopRight    :: Direction Ordinal
    BottomLeft  :: Direction Ordinal
    BottomRight :: Direction Ordinal


-- Colors

class Colorable c where
    showColor :: c -> Aeson.Object

instance Colorable (Colour Double) where
    showColor c = unwords ["rgb", show r, show g, show b] where
        Data.Colour.SRGB.Linear.RGB r g b = Data.Colour.SRGB.Linear.toRGB c

instance Colorable (AlphaColour Double) where
    showColor ac = unwords ["rgba", show r, show g, show b] where
        a = alphaChannel ac
        c = colourChannel ac
        Data.Colour.SRGB.Linear.RGB r g b = Data.Colour.SRGB.Linear.toRGB c

colorToObject :: Colorable c => c -> Aeson.Object
colorToObject = fromString . showColor


-- Background

data BackgroundType
    = BACKGROUND_COLOR
    | BACKGROUND_IMAGE
    | BACKGROUND_POSITION
    | BACKGROUND_REPEAT

data Background :: BackgroundType -> * where
    BackgroundColor    :: Color -> Background BACKGROUND_COLOR
    BackgroundImage    :: Text -> Background BACKGROUND_IMAGE
    -- BackgroundPosition :: Background BACKGROUND_POSITION
    -- BackgroundRepeat   :: Background BACKGROUND_REPEAT

backgroundToStyle :: Background a -> Style
backgroundToStyle (BackgroundColor c) = Style
    (H.singleton "background-color" (colorToObject c))
backgroundToStyle (BackgroundImage i) = Style
    (H.singleton "background-image" (String i))

background_ :: [ Background a ] -> Style
background_ attrs = Foldable.foldMap backgroundToStyle

-- backgroundColor_
-- backgroundImage_
-- backgroundPosition_ -- also x / y varieties
-- backgroundRepeat_


-- Border

data BorderType
    = BORDER_COLOR
    | BORDER_STYLE
    | BORDER_WIDTH
    | BORDER_COLLAPSE
    | BORDER_SPACING

data BorderStyle
    = None
    | Hidden
    | Dotted
    | Dashed
    | Solid
    | Double
    | Groove
    | Ridge
    | Inset
    | Outset

borderStyleText :: BorderStyle -> Text
borderStyleText None = "none"
borderStyleText Hidden = "hidden"
borderStyleText Dotted = "dotted"
borderStyleText Dashed = "dashed"
borderStyleText Solid = "solid"
borderStyleText Double = "double"
borderStyleText Groove = "groove"
borderStyleText Ridge = "ridge"
borderStyleText Inset = "inset"
borderStyleText Outset = "outset"


data Width
    = BorderBox
    -- | PxWidth Int -- could generalize to em, rem, etc
    -- | PercentWidth Int

    -- hack - get the browser to parse this for us
    | StringWidth__ Text

instance IsString Width where
    -- bleh. don't feel like writing a parser for this
    fromString = StringWidth__ . fromString

widthText :: Width -> Text
widthText BorderBox = "border-box"
widthText (StringWidth__ str) = str


data Border :: BorderType -> * where
    BorderColor :: Color       -> Border BORDER_COLOR
    BorderStyle :: BorderStyle -> Border BORDER_STYLE
    BorderWidth :: Width       -> Border BORDER_WIDTH
    -- BorderCollapse :: Border BORDER_COLLAPSE
    -- BorderSpacing :: Border BORDER_SPACING

borderToStyle :: Border a -> Style
borderToStyle (BorderColor c) = Style
    (H.singleton "border-color" (String (showColor c)))
borderToStyle (BorderStyle s) = Style
    (H.singleton "border-style" (String (borderStyleText s)))
borderToStyle (BorderWidth w) = Style
    (H.singleton "border-width" (String (widthText w)))

border_ :: [ Border a ] -> Style
border_ attrs = Foldable.foldMap borderToStyle

-- borderCollapse_
-- borderColor_
-- borderSpacing_
-- borderStyle_
-- borderWidth_
-- borderLeft_
-- borderRight_
-- borderTop_
-- borderBottom

-- direction_ (ltr or rtl)


-- Display

-- display_


-- Flex

data Direction
    = Row
    | RowReverse
    | Column
    | ColumnReverse

data FlexType
    = FLEX_DIRECTION
    | FLEX_GROW

data Flex :: FlexType -> * where
    FlexDirection :: Direction -> Flex FLEX_DIRECTION
    FlexGrow :: Int -> Flex FLEX_GROW

flex_ :: [ Flex a ] -> Style

flexDirection_ :: Direction -> Style


-- Font

data FontType
    = FONT_FAMILY

data Font :: FontType -> * where
    -- FontSize ::
    FontFamily :: Text -> Font FONT_FAMILY
    -- FontWeight

font_ :: [ Font a ] -> Style

-- height_

