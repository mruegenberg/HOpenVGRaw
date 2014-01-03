{-# LANGUAGE ForeignFunctionInterface #-}
-- This module corresponds to section 7 and 10 of the OpenVG specification.
module Graphics.Rendering.ShivaVG.Raw.Image
       (
         -- * Scissoring, Masking, and Clearing. 
         -- not implemented in ShivaVG:
         {-
         MaskOperation(..)
       , mask
       , masking
       , scissoring
         -}
         clear
         -- * Images
         -- ** Image Formats
       , ImageFormat(..)
         -- ** Creating and Destroying Images
       , Image
       , ImageQuality(..)
       , imageQuality
       , createImage
       , destroyImage
         -- ** Querying Images
       , imageFormat
       , imageWidth
       , imageHeight
         -- ** Reading and Writing Image Pixels
       , clearImage
         -- not implemented in ShivaVG
         {-
         -- ** Child Images
       , childImage
       , getParent
         -}
         -- ** Copying Pixels Between Images
       , copyImage
         -- ** Drawing Images to the Drawing Surface
       , ImageMode(..)
       , imageMode
       , drawImage
         -- ** Reading and Writing Drawing Surface Pixels
       , setPixels
         -- * Image Filters
       , ImageChannel
       , BlendMode
       , blendMode
       , TilingMode
       ) where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
import Graphics.Rendering.ShivaVG.Raw.Unsafe
import Graphics.Rendering.ShivaVG.Raw.Internal
import Graphics.Rendering.ShivaVG.Raw.Params

------------- Masks -------------

-- {{{

{-
-- | Defines the set of possible operations that may be used to modify the drawing surface alpha mask, possibly making use of a new mask image. Each operation occurs within a rectangular region of interest.
{#enum VGMaskOperation as MaskOperation {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Modifies the alpha mask values according to a given operation, possibly using alpha values taken from a mask image. If no alpha mask is configured, this has no effect.
{#fun vgMask as mask
    { imageHandle `Image' -- ^ mask
    , fromEnum' `MaskOperation' -- ^ operation
    , `Int' -- ^ x
    , `Int' -- ^ y
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

masking :: Param Bool
masking = boolParam Masking

scissoring :: Param Bool
scissoring = boolParam Scissoring
-}
      
-- | Fills the portion of the drawing surface intersecting the rectangle extending from pixel (x, y) and having the given width and height with a constant color value, taken from the ClearColor parameter.
{#fun vgClear as clear 
    { `Int' -- ^ x
    , `Int' -- ^ y
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

-- }}}

------------- Images -------------
-- {{{

-- | Defines the set of supported pixel formats and color spaces for images
-- | `A` denotes an alpha channel, `R` denotes red, `G` denotes green, and `B` denotes blue. `X` denotes a padding byte that is ignored. `L` denotes grayscale, and `BW` denotes (linear) bi-level grayscale (black-and-white), with `0` representing black and `1` representing white in either case. An `s` represents a non-linear, perceptually-uniform color space, as in sRGB and sL; an `l` represents a linear color space using the sRGB primaries. Formats with a suffix of `Pre` store pixel values in premultiplied format.
{#enum VGImageFormat as ImageFormat {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Defines varying levels of resampling quality to be used when drawing images.
{#enum VGImageQuality as ImageQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

imageQuality :: Param ImageQuality
imageQuality = enumParam ImageQuality

-- | Creates an image with the given width, height, and pixel format and returns a handle to it.
{#fun vgCreateImage as createImage 
    { fromEnum' `ImageFormat' -- ^ format
    , `Int' -- ^ width
    , `Int' -- ^ height 
    , combineOptions `[ImageQuality]' -- ^ allowedQuality
    } -> `Image' Image #}

-- | The resources associated with an image are deallocated. Following the call, the image handle is no longer valid in any context that shared it. 
{#fun vgDestroyImage as destroyImage 
    { imageHandle `Image' -- ^ image
    } -> `()' #}

{#enum VGImageParamType as ImageParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

imageFormat :: Image -> Param ImageFormat
imageFormat (Image p) = hEnumParam p ImageFormat

imageWidth :: Image -> Param Float
imageWidth (Image p) = hFloatParam p ImageWidth

imageHeight :: Image -> Param Float
imageHeight (Image p) = hFloatParam p ImageHeight

-- | Fills a given rectangle of an image with the color specified by the `ClearColor` parameter. The rectangle to be cleared is given by x, y, width, and height, which must define a positive region. The rectangle is clipped to the bounds of the image.
{#fun vgClearImage as clearImage 
    { imageHandle `Image' -- ^ image
    , `Int' -- ^ x
    , `Int' -- ^ y 
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

-- TODO:
{-
-- | Reads pixel values from memory, performs format conversion if necessary, and stores the resulting pixels into a rectangular portion of an image.
VG_API_CALL void vgImageSubData(VGImage image,
                                const void * data, VGint dataStride,
                                VGImageFormat dataFormat,
                                VGint x, VGint y, VGint width, VGint height);
VG_API_CALL void vgGetImageSubData(VGImage image,
                                   void * data, VGint dataStride,
                                   VGImageFormat dataFormat,
                                   VGint x, VGint y,
                                   VGint width, VGint height);
-}

-- not implemented in ShivaVG:
{-
-- | Returns a new image handle that refers to a portion of the parent image. The region is given by the intersection of the bounds of the parent image with the rectangle beginning at pixel (x, y) with dimensions width and height, which must define a positive region contained entirely within parent.
{#fun vgChildImage as childImage 
    { imageHandle `Image' -- ^ parent
    , `Int' -- ^ x
    , `Int' -- ^ y
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `Image' Image #}

-- | Returns the parent of the given image. If image has no parent, image is returned.
{#fun vgGetParent as getParent 
    { imageHandle `Image' -- ^ image
    } -> `Image' Image #}
-}

-- | The source image pixel (sx + i, sy + j) is copied to the destination image pixel (dx +i,dy +j),for 0 ≤ i <widthand 0 ≤ j <height. Pixels whose source or destination lie outside of the bounds of the respective image are ignored. Pixel format conversion is applied as needed.
-- 
-- If the `dither` flag is `True`, an implementation-dependent dithering algorithm may be applied. This may be useful when copying into a destination image with a smaller color bit depth than that of the source image.
-- 
-- If src and dst are the same image, or have a common ancestor and thus share storage, the copy will occur in a consistent fashion as though the source pixels were first copied into a temporary buffer and then copied from the temporary buffer to the destination.
{#fun vgCopyImage as copyImage
    { imageHandle `Image' -- ^ dst
    , `Int' -- ^ dx
    , `Int' -- ^ dy
    , imageHandle `Image' -- ^ src
    , `Int' -- ^ dx
    , `Int' -- ^ dy
    , `Int' -- ^ width
    , `Int' -- ^ height
    , `Bool' -- ^ dither
    } -> `()' #}

-- | Used to select between several styles of image drawing.
{#enum VGImageMode as ImageMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

imageMode :: Param ImageMode
imageMode = enumParam ImageMode

-- Draw an image may be drawn to the current drawing surface.
{#fun vgDrawImage as drawImage 
    { imageHandle `Image' -- ^ image
    } -> `()' #}

-- | Copies pixel data from the image src onto the drawing surface.
-- 
-- The image pixel (sx + i, sy + j) is copied to the drawing surface pixel (dx+ i,dy+ j),for 0 ≤ i <widthand 0 ≤ j <height. Pixels whose source lies outside of the bounds of src or whose destination lies outside the bounds of the drawing surface are ignored.
{#fun vgSetPixels as setPixels 
    { `Int' -- ^ dx
    , `Int' -- ^ dy
    , imageHandle `Image' -- ^ src
    , `Int' -- ^ sx
    , `Int' -- ^ sy
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

-- TODO
{-
VG_API_CALL void vgWritePixels(const void * data, VGint dataStride,
                               VGImageFormat dataFormat,
                               VGint dx, VGint dy,
                               VGint width, VGint height);
VG_API_CALL void vgGetPixels(VGImage dst, VGint dx, VGint dy,
                             VGint sx, VGint sy,
                             VGint width, VGint height);
VG_API_CALL void vgReadPixels(void * data, VGint dataStride,
                              VGImageFormat dataFormat,
                              VGint sx, VGint sy,
                              VGint width, VGint height);
VG_API_CALL void vgCopyPixels(VGint dx, VGint dy,
                              VGint sx, VGint sy,
                              VGint width, VGint height);
-}

-- }}}

------------- Image Filters -------------

{#enum VGImageChannel as ImageChannel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGBlendMode as BlendMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

blendMode :: Param BlendMode
blendMode = enumParam BlendMode

-- | Defines possible methods for defining colors for source pixels that lie outside the bounds of the source image.
{#enum VGTilingMode as TilingMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- none of the image filter stuff is implemented in ShivaVG, so we leave it out.
{-

filterFormatLinear :: Param Bool
filterFormatLinear = boolParam FilterFormatLinear

filterFormatPremultiplied :: Param Bool
filterFormatPremultiplied = boolParam FilterFormatPremultiplied

filterChannelMask :: Param [ImageChannel]
filterChannelMask = combinedOptsParam FilterChannelMask [Red,Green,Blue,Alpha]

{#fun vgColorMatrix 
    { imageHandle `Image'
    , imageHandle `Image'
    , `Ptr CFloat'
    } -> `()' #}

colorMatrix :: Image -- ^ dst
            -> Image -- ^ src
            -> Matrix Float -- ^ matrix
            -> IO ()
colorMatrix dst src matrix = vgColorMatrix dst src matrixPtr
  where matrixPtr = undefined -- TODO

{#fun vgConvolve 
    { imageHandle `Image'
    , imageHandle `Image'
    , `Int'
    , `Int'
    , `Int'
    , `Int'
    , `Ptr CFloat'
    , `Float'
    , `Float'
    , fromEnum' `TilingMode'
    } -> `()' #}

convolve :: Image -- ^ dst
         -> Image -- ^ src
         -> (Int,Int) -- ^ kernel size
         -> (Int,Int) -- ^ shift x y
         -> Matrix Float -- ^ kernel
         -> Float -- ^ scale
         -> Float -- ^ bias
         -> Tilingmode -- ^ tilingMode
         -> IO ()
convolve dst src (kWidth,kHeight) (sX,sY) kernelMatrix scale bias tilingMode =
  vgConvolve dst src kWidth kHeight sX sY kernelPtr scale bias tilingMode
    where
      kernelPtr = undefined -- TODO
{-

VG_API_CALL void vgSeparableConvolve(VGImage dst, VGImage src,
                                     VGint kernelWidth,
                                     VGint kernelHeight,
                                     VGint shiftX, VGint shiftY,
                                     const VGshort * kernelX,
                                     const VGshort * kernelY,
                                     VGfloat scale,
                                     VGfloat bias,
                                     VGTilingMode tilingMode);
VG_API_CALL void vgGaussianBlur(VGImage dst, VGImage src,
                                VGfloat stdDeviationX,
                                VGfloat stdDeviationY,
                                VGTilingMode tilingMode);
VG_API_CALL void vgLookup(VGImage dst, VGImage src,
                          const VGubyte * redLUT,
                          const VGubyte * greenLUT,
                          const VGubyte * blueLUT,
                          const VGubyte * alphaLUT,
                          VGboolean outputLinear,
                          VGboolean outputPremultiplied);
VG_API_CALL void vgLookupSingle(VGImage dst, VGImage src,
                                const VGuint * lookupTable,
                                VGImageChannel sourceChannel,
                                VGboolean outputLinear,
                                VGboolean outputPremultiplied);
-}
-}