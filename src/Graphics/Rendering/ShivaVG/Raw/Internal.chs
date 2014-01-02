{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw.Internal where

#include <vg/openvg.h>

import Foreign.C.Types
import Foreign.Ptr
import Data.Bits((.|.))
import Control.Monad(liftM)

-- this module contains direct equivalents for OpenVG types
-- these are only used internally and are mostly 
-- directly taken from the typedefs in openvg.h

---------------- Basic types ----------------

type VGfloat = {#type VGfloat#} -- CFloat
type VGbyte  = {#type VGbyte#}  -- CChar
type VGubyte = {#type VGubyte#} -- CUChar
type VGshort = {#type VGshort#} -- CShort
type VGint   = {#type VGint#}    -- CInt
type VGuint  = {#type VGuint#}  -- CUInt
type VGbitfield = {#type VGbitfield#} -- CUInt

{#enum VGboolean {underscoreToCase} deriving (Show, Eq)#}

vgBool2Bool :: VGboolean -> Bool
vgBool2Bool VgTrue = True
vgBool2Bool VgFalse = False

mVGBool2Bool :: IO VGboolean -> IO Bool
mVGBool2Bool = liftM vgBool2Bool

bool2vgBool :: Bool -> VGboolean
bool2vgBool True = VgTrue
bool2vgBool False = VgFalse

type VGHandle = {#type VGHandle#}

combineOptions :: (Enum a) => [a] -> VGbitfield
combineOptions = fromIntegral . (foldr ((.|.) . fromEnum) 0)

fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

mToEnum :: Enum a => IO Int -> IO a
mToEnum = liftM toEnum

toEnum' :: (Enum a) => CInt -> a
toEnum' = toEnum . fromIntegral



-- we need to define these here, since both Image and Paint need access

-- | Image handle
newtype Image = Image { imageHandle :: VGHandle }

-- | Paint handle
newtype Paint = Paint { paintHandle :: VGHandle }