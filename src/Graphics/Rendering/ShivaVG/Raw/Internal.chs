{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw.Internal where

#include <vg/openvg.h>

import Foreign.C.Types
import Foreign.Ptr

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

type VGHandle = {#type VGHandle#}