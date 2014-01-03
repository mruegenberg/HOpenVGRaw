{-# LANGUAGE ForeignFunctionInterface #-}
-- | Section 9 of the specification
module Graphics.Rendering.ShivaVG.Raw.Paint 
       (
         -- * Paint
         -- ** Paint Definitions
         Paint
       , createPaint
       , destroyPaint
       , setPaint
       -- , getPaint
       , paintType
       , SRGBA(..)
       , paintColor
       , clearColor
       , ColorRampSpreadMode(..)
       , colorRampSpreadMode
       , colorRampPremultiplied
       , colorRampStops
         -- ...
       , TilingMode(..)
       , patternTilingMode
         -- ** Color Paint
       , SRGBA8(..)
       -- , setColor
       -- , getColor
         -- ** Pattern Paint
        , paintPattern
       ) where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
import Graphics.Rendering.ShivaVG.Raw.Image
import Graphics.Rendering.ShivaVG.Raw.Paths
import Graphics.Rendering.ShivaVG.Raw.Internal
import Graphics.Rendering.ShivaVG.Raw.Unsafe
import Graphics.Rendering.ShivaVG.Raw.Params

import Data.Word(Word8)

-- | Creates a new paint object that is initialized to a set of default values and returns a handle to it.
{#fun vgCreatePaint as createPaint { } -> `Paint' Paint #}

-- | The resources associated with the are deallocated. Following the call, the paint handle is no longer valid in any of the contexts that shared it.
{#fun vgDestroyPaint as destroyPaint 
    { paintHandle `Paint' -- ^ paint
    } -> `()' #}

-- | Set a paint definition on the current context. The current paint replaces the previously set paint object, if any, for the given paint mode or modes.
{#fun vgSetPaint as setPaint 
    { paintHandle `Paint'
    , combineOptions `[PaintMode]' 
    } -> `()' #}

-- | Returns the paint object currently set for the given paintMode
{#fun vgGetPaint as getPaint 
    { fromEnum' `PaintMode' -- ^ paintMode
    } -> `Paint' Paint #}

{#enum VGPaintParamType as PaintParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Determines the type of paint to be applied
{#enum VGPaintType as PaintType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Paint type
paintType :: Paint -> Param PaintType
paintType (Paint p) = hEnumParam p PaintType

-- | An sRGBA color, with components as values between 0.0 and 1.0
data SRGBA = SRGBA { red :: !Float, green :: !Float, blue :: !Float, alpha :: !Float } deriving (Eq, Show)

paintColor :: Paint -> Param SRGBA
paintColor (Paint p) = hFVParam p PaintColor (\(SRGBA r g b a) -> [r,g,b,a]) (\[r,g,b,a] -> SRGBA r g b a)

clearColor :: Param SRGBA
clearColor = fvParam ClearColor (\(SRGBA r g b a) -> [r,g,b,a]) (\[r,g,b,a] -> SRGBA r g b a)

-- | The application may only define stops with offsets between 0 and 1. Spread modes define how the given set of stops are repeated or extended in order to define interpolated color values for arbitrary input values outside the [0,1] range.
-- 
-- - `ColorRampSpreadPad`: extend stops
--
-- - `ColorRampSpreadRepeat`: repeat stops
-- 
-- - `ColorRampSpreadReflect`: repeat stops in reflected order
{#enum VGColorRampSpreadMode as ColorRampSpreadMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

colorRampSpreadMode :: Paint -> Param ColorRampSpreadMode
colorRampSpreadMode (Paint p) = hEnumParam p PaintColorRampSpreadMode

colorRampPremultiplied :: Paint -> Param Bool
colorRampPremultiplied (Paint p) = hBoolParam p PaintColorRampPremultiplied

colorRampStops :: Paint -> Param [Float]
colorRampStops (Paint p) = hFVParam p PaintColorRampStops id id

-- TODO: paintLinearGradient
-- TODO: paintRadialGradient

patternTilingMode :: Paint -> Param TilingMode
patternTilingMode (Paint p) = hEnumParam p PaintPatternTilingMode

-- | A shorthand for sRGBA colors, with values between 0 and 255 for each channel
data SRGBA8 = SRGBA8 { red8 :: !Word8, green8 :: !Word8, blue8 :: !Word8, alpha8 :: !Word8 } deriving (Eq, Show)
sRGBA2Int :: SRGBA8 -> CUInt
sRGBA2Int (SRGBA8 r g b a) = foldr (.|.) 0 shifted
  where
    shifted = zipWith (\i v -> shift v (i * 8)) [3,2,1,0] channels
    channels = map fromIntegral [r,g,b,a]
    
int2sRGBA :: CUInt -> SRGBA8
int2sRGBA col = SRGBA8 r g b a
  where [r,g,b,a] = map (\i -> fromIntegral (shift col (- i * 8)) :: Word8) [3,2,1,0]

{#fun vgSetColor as setColor 
    { paintHandle `Paint' -- ^ paint
    , sRGBA2Int `SRGBA8' -- ^ rgba
    } -> `()' #}

{#fun vgGetColor as getColor
    { paintHandle `Paint' -- ^ paint
    } -> `SRGBA8' int2sRGBA #}

-- | Replaces any previous pattern image defined on the given paint object for the given set of paint modes with a new pattern image
{#fun vgPaintPattern as paintPattern 
    { paintHandle `Paint' -- ^ paint
    , imageHandle `Image' -- ^ pattern
    } -> `()' #}

