{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw
       ( 
         -- * ShivaVG extensions
         createContextSH
       , resizeSurfaceSH
       , destroyContextSH
         -- * Modules
       , module Graphics.Rendering.ShivaVG.Raw.Types
       , module Graphics.Rendering.ShivaVG.Raw.Paths
       , module Graphics.Rendering.ShivaVG.Raw.Image
         -- * Errors
       , ErrorCode(..)
       , getError
         -- * Rendering Quality and Antialiasing
       , RenderingQuality(..)
       , PixelLayout(..)
         -- * Querying Hardware Capabilities
       , HardwareQueryType(..)
       , HardwareQueryResult(..)
       , hardwareQueryImageFormat
       , hardwareQueryPathDatatype
         -- * Extension Information
       , getString
         -- * Forcing drawing to complete
       , vgFlush
       , vgFinish
       )
       where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
import Foreign.C.String(peekCString)
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}
{#import Graphics.Rendering.ShivaVG.Raw.Types#}
{#import Graphics.Rendering.ShivaVG.Raw.Paths#}
{#import Graphics.Rendering.ShivaVG.Raw.Image#}
{#import Graphics.Rendering.ShivaVG.Raw.Paint#}

---------------- ShivaVG extensions ----------------
-- {{{

int2Bool = toEnum . fromEnum

{#fun vgCreateContextSH as createContextSH
     { `Int' -- ^ width
     , `Int' -- ^ height
     } -> `Bool' int2Bool #}

{#fun vgResizeSurfaceSH as resizeSurfaceSH 
    { `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

{#fun vgDestroyContextSH as destroyContextSH {} -> `()' #}

-- }}}


---------------- Errors ----------------

{#enum VGErrorCode as ErrorCode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgGetError as getError {} -> `ErrorCode' toEnum' #}

------------- Rendering settings -------------

-- | Default: `RenderingQualityBetter`
{#enum VGRenderingQuality as RenderingQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Describes a number of possible geometric layouts of the red, green, and blue emissive or reflective elements within a pixel. This information may be used as a hint to the rendering engine to improve rendering quality.
{#enum VGPixelLayout as PixelLayout {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Hardware queries -------------

{#enum VGHardwareQueryType as HardwareQueryType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGHardwareQueryResult as HardwareQueryResult {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


{#fun vgHardwareQuery 
    { fromEnum' `HardwareQueryType' -- ^ key
    , `Int' -- ^ setting
    } -> `HardwareQueryResult' toEnum' #}
hardwareQueryImageFormat :: ImageFormat -- ^ setting
                         -> IO HardwareQueryResult
hardwareQueryImageFormat setting = vgHardwareQuery ImageFormatQuery (fromEnum setting)
hardwareQueryPathDatatype :: PathDatatype -- ^ setting
                          -> IO HardwareQueryResult
hardwareQueryPathDatatype setting = vgHardwareQuery PathDatatypeQuery (fromEnum setting)

------------- Extension Information -------------

{#enum VGStringID {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgGetString 
    { fromEnum' `VGStringID' -- ^ name
    } -> `Ptr CChar' castPtr #}
getString :: VGStringID -> IO String
getString name = vgGetString name >>= peekCString

------------- Forcing drawing to complete -------------

-- | Ensures that all outstanding requests on the current context will complete in finite time. May return prior to the actual completion of all requests.
{#fun vgFlush {} -> `()' #}

-- | Forces all outstanding requests on the current context to complete, returning only when the last request has completed.
{#fun vgFinish {} -> `()' #}

