{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw.Types 
       (
         -- * Matrix Manipulation
         MatrixMode(..)
       , loadIdentity  
       , loadMatrix
       , getMatrix
       , multMatrix
       , translate
       , scale
       , shear
       , rotate
       ) where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
{#import Graphics.Rendering.ShivaVG.Raw.Paths#}
{#import Graphics.Rendering.ShivaVG.Raw.Image#}
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}

import Data.Packed.Matrix(Matrix,(><))
import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import Data.Packed.Development(unsafeToForeignPtr,unsafeFromForeignPtr)
import Control.Exception(assert)

import Data.Word(Word8)

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


------------- Image Filters -------------

{#enum VGImageChannel as ImageChannel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGBlendMode as BlendMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{-
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
