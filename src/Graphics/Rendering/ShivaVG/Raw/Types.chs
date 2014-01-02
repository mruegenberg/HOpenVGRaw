{-# LANGUAGE ForeignFunctionInterface #-}
-- | Types for OpenVG
module Graphics.Rendering.ShivaVG.Raw.Types 
       (
         -- * Errors
         ErrorCode(..)
       , getError
         -- * Rendering settings
       , RenderingQuality(..)
       , PixelLayout(..)
         -- * Matrix Manipulation
       , MatrixMode(..)
       , loadIdentity  
       , loadMatrix
       , getMatrix
       , multMatrix
       , translate
       , scale
       , shear
       , rotate
         -- * Masking and Clearing
       , MaskOperation(..)
       , mask
       , clear
         -- * Paths
         -- ** The Standard Path Format
       , PathFormat(..)
       , PathDatatype(..)
         -- we leave out PathAbsRel and PathSegment, since both are contained in PathCommand
       , PathCommand(..)
         -- ** Path Operations
       , Path
       , PathCapability(..)
       , createPath
       , clearPath
       , destroyPath
         -- ...
       , getPathCapabilities
       , removePathCapabilities
       , appendPath
       , appendPathData
       , closePath
         -- ...
       , transformPath
       , interpolatePath
       , pathLength
         -- ...
         -- ** Interpretation of Paths
       , CapStyle(..)
       , JoinStyle(..)
       , FillRule(..)
       , PaintMode(..)
       , drawPath
         -- * Paint
         -- ** Paint Definitions
       , Paint
       , createPaint
       , destroyPaint
       , setPaint
       , getPaint
         -- ...
         -- ** Color Paint
       , SRGBA(..)
       , setColor
       , getColor
         -- ** Gradient Paint
       , ColorRampSpreadMode(..)
         -- ** Pattern Paint
       , paintPattern
       , TilingMode(..)
         -- * Images
         -- ** Image Formats
       , ImageFormat(..)
         -- ** Creating and Destroying Images
       , Image
       , ImageQuality(..)
       , createImage
       , destroyImage
         -- ** Querying Images
         -- ...
         -- ** Reading and Writing Image Pixels
       , clearImage
         -- ** Child Images
       , childImage
       , getParent
         -- ** Copying Pixels Between Images
       , copyImage
         -- ** Drawing Images to the Drawing Surface
       , ImageMode(..)
       , drawImage
         -- ** Reading and Writing Drawing Surface Pixels
       , setPixels
       ) where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
import Foreign.C.String(peekCString)
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}

import Data.Packed.Matrix(Matrix,(><))
import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import Data.Packed.Development(unsafeToForeignPtr,unsafeFromForeignPtr)
import Control.Exception(assert)

import Data.Word(Word8)

toEnum' :: (Enum a) => CInt -> a
toEnum' = toEnum . fromIntegral
fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

combineOptions :: (Enum a) => [a] -> VGbitfield
combineOptions = fromIntegral . (foldr ((.|.) . fromEnum) 0)

{#enum VGErrorCode as ErrorCode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


{#fun vgGetError as getError {} -> `ErrorCode' toEnum' #}

------------- Rendering settings -------------
-- | Default: `RenderingQualityBetter`
{#enum VGRenderingQuality as RenderingQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Describes a number of possible geometric layouts of the red, green, and blue emissive or reflective elements within a pixel. This information may be used as a hint to the rendering engine to improve rendering quality.
{#enum VGPixelLayout as PixelLayout {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Matrix Manipulation -------------

-- | The current matrix to be manipulated is specified by setting the matrix mode. Separate matrices are maintained for transforming paths, images, and paint (gradients and patterns). 
{#enum VGMatrixMode as MatrixMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Sets the current matrix M to the identity matrix
{#fun vgLoadIdentity as loadIdentity {} -> `()' #}

{#fun vgLoadMatrix
    { id `Ptr CFloat'
    } -> `()' #}
-- | Loads an arbitrary set of matrix values into the current matrix
loadMatrix :: Matrix Float -> IO ()
-- we don't directly use the `Storable` instance of Matrix, but go via `flatten`
-- because that ensures that the elements are in the right order.
loadMatrix m = assert (Matrix.rows m == 3 && Matrix.cols m == 3) $
               withForeignPtr ptr' vgLoadMatrix
  where 
    (ptr',_,_) = unsafeToForeignPtr elemVector
    elemVector = Vector.mapVector (realToFrac :: Float -> CFloat) (Matrix.flatten m)

{#fun vgGetMatrix
    { id `Ptr CFloat'
    } -> `()' #}

-- | Retrieve the value of the current transformation
getMatrix :: IO (Matrix Float)
getMatrix = allocaArray (3 * 3) $ \ptr ->
  do
    vgGetMatrix ptr
    lst <- peekArray (3 * 3) ptr
    return $ (3><3) $ map realToFrac lst

{#fun vgMultMatrix
    { id `Ptr CFloat'
    } -> `()' #}

-- | Right-multiplies the current matrix M by a given matrix
multMatrix :: Matrix Float -> IO ()
multMatrix m = assert (Matrix.rows m == 3 && Matrix.cols m == 3) $
               withForeignPtr ptr' vgMultMatrix
  where
    (ptr',_,_) = unsafeToForeignPtr elemVector
    elemVector = Vector.mapVector (realToFrac :: Float -> CFloat) (Matrix.flatten m)

{#fun vgTranslate as translate 
    { `Float' -- ^ tx
    , `Float' -- ^ ty
    } -> `()' #}

{#fun vgScale as scale 
    { `Float' -- ^ sx
    , `Float' -- ^ sy
    } -> `()' #}

{#fun vgShear as shear 
    { `Float' -- ^ shx
    , `Float' -- ^ shy
    } -> `()' #}

{#fun vgRotate as rotate 
    { `Float' -- ^ angle
    } -> `()' #}


------------- Masking and Clearing -------------
-- {{{

{#enum VGMaskOperation as MaskOperation {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgMask as mask
    { imageHandle `Image' -- ^ mask
    , fromEnum' `MaskOperation' -- ^ operation
    , `Int' -- ^ x
    , `Int' -- ^ y
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}
      

{#fun vgClear as clear 
    { `Int' -- ^ x
    , `Int' -- ^ y
    , `Int' -- ^ width
    , `Int' -- ^ height
    } -> `()' #}

-- }}}

------------- Paths -------------

-- | The standard path format. This is virtually always what you pass to path functions.
{#enum define PathFormat { VG_PATH_FORMAT_STANDARD as PathFormatStandard } deriving (Show, Eq) #}

-- | Possible numerical datatypes for path coordinate data.
{#enum VGPathDatatype as PathDatatype {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathAbsRel as PathAbsRel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathSegment as PathSegment {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Path segment commands, in absolute and relative variants
{#enum VGPathCommand as PathCommand {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Path Handle
newtype Path = Path { pathHandle :: VGHandle }

-- | Set of constants specifying which operations may be performed on a given path object. At the time a path is defined, the application specifies which operations it wishes to be able to perform on the path. Over time, the application may disable previously enabled capabilities, but it may not re-enable capabilities once they have been disabled. This feature allows OpenVG implementations to make use of internal path representations that may not support all path operations, possibly resulting in higher performance on paths where those operations will not be performed.
{#enum VGPathCapabilities as PathCapability {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | creates a new path that is ready to accept segment data and returns a handle to it.
{#fun vgCreatePath as createPath
    { fromEnum' `PathFormat' -- ^ pathFormat
    , fromEnum' `PathDatatype' -- ^ datatype
    , `Float' -- ^ scale
    , `Float' -- ^ bias
    , `Int' -- ^ segmentCapacityHint
    , `Int' -- ^ coordCapacityHint
    , combineOptions `[PathCapability]' -- ^ capabilities
    } -> `Path' Path #}

-- | Removes all segment command and coordinate data associated with a path. The handle continues to be valid for use in the future, and the path format and datatype retain their existing values.
{#fun vgClearPath as clearPath
    { pathHandle `Path' -- ^ path
    , combineOptions `[PathCapability]' -- ^ capabilities
    } -> `()' #}

-- | releases any resources associated with path, and makes the handle invalid in all contexts that shared it.
{#fun vgDestroyPath as destroyPath
    { pathHandle `Path' -- ^ path
    } -> `()' #}

-- TODO:
-- pack so that vgGetParameter is used, and can be safely called from outside
{#enum VGPathParamType as PathParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO: more elegant
unpackPathCapabilities :: VGbitfield -> [PathCapability]
unpackPathCapabilities bits = filter ((/= 0) . (.&. bits) . fromIntegral . fromEnum)
                              [ PathCapabilityAppendFrom
                              , PathCapabilityAppendTo
                              , PathCapabilityModify
                              , PathCapabilityTransformFrom
                              , PathCapabilityTransformTo
                              , PathCapabilityInterpolateFrom
                              , PathCapabilityInterpolateTo  
                              , PathCapabilityPathLength
                              , PathCapabilityPointAlongPath
                              , PathCapabilityTangentAlongPath
                              , PathCapabilityPathBounds
                              , PathCapabilityPathTransformedBounds  
                              , PathCapabilityAll ]

-- | Current capabilities of the path.
{#fun vgGetPathCapabilities as getPathCapabilities
    { pathHandle `Path' -- ^ path
    } -> `[PathCapability]' unpackPathCapabilities #}

-- | Requests the set of capabilities specified in the capabilities argument to be disabled for the given path. Attempting to remove a capability that is already disabled has no effect.
{#fun vgRemovePathCapabilities as removePathCapabilities
    { pathHandle `Path' -- ^ path
    , combineOptions `[PathCapability]' -- ^ capabilities
    } -> `()' #}

-- | Appends a copy of all path segments from `srcPath` onto the end of the existing data in `dstPath`
{#fun vgAppendPath as appendPath
    { pathHandle `Path' -- ^ dstPath
    , pathHandle `Path' -- ^ srcPath
    } -> `()' #}

{#fun vgAppendPathData
    { pathHandle `Path' -- ^ dstPath
    , `Int' -- ^ numSegments
    , id `Ptr VGubyte' -- ^ pathSegments
    , id `Ptr ()' -- ^ pathData
    } -> `()' #}
    
-- | appends data taken from a client-side representation stored in `pathData` to the given path `dstPath`.
appendPathData :: Path -> [(PathCommand, (Float, Float))] -> IO ()
appendPathData p cmds = 
  withArrayLen (map (fromEnum' . fst) cmds) $ \segs ptr1 ->
    withArray (concatMap (\(_,(x,y)) -> [x,y]) cmds) $ \ptr2 ->
      vgAppendPathData p segs ptr1 (castPtr ptr2)

-- | Close the path.
closePath :: Path -> IO ()
closePath p = withArray [fromEnum' ClosePath] $ \ptr -> 
  vgAppendPathData p 1 ptr nullPtr
  
  
-- TODO:
{-
VG_API_CALL void vgModifyPathCoords(VGPath dstPath, VGint startIndex,
                                    VGint numSegments,
                                    const void * pathData);
-}

-- | Appends a transformed copy of srcPath to the current contents of dstPath. The appended path is equivalent to the results of applying the current path-user-to-surface transformation (`MatrixPathUserToSurface`) to srcPath.
{#fun vgTransformPath as transformPath
    { pathHandle `Path' -- ^ dstPath
    , pathHandle `Path' -- ^ srcPath
    } -> `()' #}

-- | Interpolate between two paths.
-- 
-- Appends a path, defined by interpolation (or extrapolation) between the paths startPath and endPath by the given amount, to the path dstPath.
-- 
-- Returns `True` if interpolation was successful (i.e., the paths had compatible segment types after normalization), and `False` otherwise. 
{#fun vgInterpolatePath as interpolatePath
    { pathHandle `Path' -- ^ dstPath
    , pathHandle `Path' -- ^ startPath
    , pathHandle `Path' -- ^ endPath
    , `Float' -- ^ amount
    } -> `Bool' #}

-- | Returns the length of a given portion of a path in the user coordinate system (that is, in the path’s own coordinate system, disregarding any matrix settings). Only the subpath consisting of the numSegments path segments beginning with startSegment (where the initial path segment has index 0) is used. If an error occurs, -1.0f is returned.
-- The `PathCapabilityPathLength` capability must be enabled for path.
{#fun vgPathLength as pathLength
    { pathHandle `Path' -- ^ path
    , `Int' -- ^ startSegment
    , `Int' -- ^ numSegments
    } -> `Float' #}

-- TODO
{-
VG_API_CALL void vgPointAlongPath(VGPath path,
                                  VGint startSegment, VGint numSegments,
                                  VGfloat distance,
                                  VGfloat * x, VGfloat * y,
                                  VGfloat * tangentX, VGfloat * tangentY);
VG_API_CALL void vgPathBounds(VGPath path,
                              VGfloat * minX, VGfloat * minY,
                              VGfloat * width, VGfloat * height);
VG_API_CALL void vgPathTransformedBounds(VGPath path,
                                         VGfloat * minX, VGfloat * minY,
                                         VGfloat * width, VGfloat * height);
-}

-- | Defines constants for the Butt, Round, and Square end cap styles
{#enum VGCapStyle as CapStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Defines constants for the Miter, Round, and Bevel line join styles
{#enum VGJoinStyle as JoinStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Constants for the even/odd and non- zero fill rules.
{#enum VGFillRule as FillRule {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Constants for stroking and filling paths, to be used by the `drawPath`, `setPaint`, and `getPaint` functions.
{#enum VGPaintMode as PaintMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgDrawPath as drawPath
    { pathHandle `Path' -- ^ path
    , combineOptions `[PaintMode]' -- paintModes
    } -> `()' #} 

------------- Paint -------------

-- | Paint handle
newtype Paint = Paint { paintHandle :: VGHandle }

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

-- TODO:
{#enum VGPaintParamType as PaintParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO:
{#enum VGPaintType as PaintType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


-- | A shorthand for sRGBA colors, with values between 0 and 255 for each channel
data SRGBA = SRGBA { red8 :: !Word8, green8 :: !Word8, blue8 :: !Word8, alpha8 :: !Word8 } deriving (Eq, Show)
sRGBA2Int :: SRGBA -> CUInt
sRGBA2Int (SRGBA r g b a) = foldr (.|.) 0 shifted
  where
    shifted = zipWith (\i v -> shift v (i * 8)) [3,2,1,0] channels
    channels = map fromIntegral [r,g,b,a]
    
int2sRGBA :: CUInt -> SRGBA
int2sRGBA col = SRGBA r g b a
  where [r,g,b,a] = map (\i -> fromIntegral (shift col (- i * 8)) :: Word8) [3,2,1,0]

{#fun vgSetColor as setColor 
    { paintHandle `Paint' -- ^ paint
    , sRGBA2Int `SRGBA' -- ^ rgba
    } -> `()' #}

{#fun vgGetColor as getColor
    { paintHandle `Paint' -- ^ paint
    } -> `SRGBA' int2sRGBA #}

-- | The application may only define stops with offsets between 0 and 1. Spread modes define how the given set of stops are repeated or extended in order to define interpolated color values for arbitrary input values outside the [0,1] range.
-- 
-- - `ColorRampSpreadPad`: extend stops
--
-- - `ColorRampSpreadRepeat`: repeat stops
-- 
-- - `ColorRampSpreadReflect`: repeat stops in reflected order
{#enum VGColorRampSpreadMode as ColorRampSpreadMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Replaces any previous pattern image defined on the given paint object for the given set of paint modes with a new pattern image
{#fun vgPaintPattern as paintPattern 
    { paintHandle `Paint' -- ^ paint
    , imageHandle `Image' -- ^ pattern
    } -> `()' #}

-- | Defines possible methods for defining colors for source pixels that lie outside the bounds of the source image.
{#enum VGTilingMode as TilingMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Images -------------

-- | Defines the set of supported pixel formats and color spaces for images
-- | `A` denotes an alpha channel, `R` denotes red, `G` denotes green, and `B` denotes blue. `X` denotes a padding byte that is ignored. `L` denotes grayscale, and `BW` denotes (linear) bi-level grayscale (black-and-white), with `0` representing black and `1` representing white in either case. An `s` represents a non-linear, perceptually-uniform color space, as in sRGB and sL; an `l` represents a linear color space using the sRGB primaries. Formats with a suffix of `Pre` store pixel values in premultiplied format.
{#enum VGImageFormat as ImageFormat {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

newtype Image = Image { imageHandle :: VGHandle }

-- | Defines varying levels of resampling quality to be used when drawing images.
{#enum VGImageQuality as ImageQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

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

-- TODO:
{#enum VGImageParamType as ImageParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

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

{#enum VGImageChannel as ImageChannel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGBlendMode as BlendMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


/* Image Filters */
VG_API_CALL void vgColorMatrix(VGImage dst, VGImage src,
                               const VGfloat * matrix);
VG_API_CALL void vgConvolve(VGImage dst, VGImage src,
                            VGint kernelWidth, VGint kernelHeight,
                            VGint shiftX, VGint shiftY,
                            const VGshort * kernel,
                            VGfloat scale,
                            VGfloat bias,
                            VGTilingMode tilingMode);
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

------------- Renderer and Extension Information -------------

{#enum VGStringID {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgGetString 
    { fromEnum' `VGStringID' -- ^ name
    } -> `Ptr CChar' castPtr #}
getString :: VGStringID -> IO String
getString name = vgGetString name >>= peekCString

------------- General -------------

{#fun vgFlush {} -> `()' #}

{#fun vgFinish {} -> `()' #}

