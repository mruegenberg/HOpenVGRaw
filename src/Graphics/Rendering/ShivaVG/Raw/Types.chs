{-# LANGUAGE ForeignFunctionInterface #-}
-- | Types for OpenVG
module Graphics.Rendering.ShivaVG.Raw.Types where

#include <vg/openvg.h>

import Foreign
import Foreign.C.Types
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}

toEnum' = toEnum . fromIntegral
fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

{#enum VGErrorCode as ErrorCode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


{#fun vgGetError as getError {} -> `ErrorCode' toEnum' #}

------------- Rendering settings -------------
-- | Default: `RenderingQualityBetter`
{#enum VGRenderingQuality as RenderingQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPixelLayout as PixelLayout {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Matrices -------------
{#enum VGMatrixMode as MatrixMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO: Matrix Manipulation
{-
VG_API_CALL void vgLoadIdentity(void);
VG_API_CALL void vgLoadMatrix(const VGfloat * m);
VG_API_CALL void vgGetMatrix(VGfloat * m);
VG_API_CALL void vgMultMatrix(const VGfloat * m);
VG_API_CALL void vgTranslate(VGfloat tx, VGfloat ty);
VG_API_CALL void vgScale(VGfloat sx, VGfloat sy);
VG_API_CALL void vgShear(VGfloat shx, VGfloat shy);
VG_API_CALL void vgRotate(VGfloat angle);
-}

------------- Paths -------------
{#enum VGMaskOperation as VGMaskOperation {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO:
{-
/* Masking and Clearing */
VG_API_CALL void vgMask(VGImage mask, VGMaskOperation operation,
                        VGint x, VGint y, VGint width, VGint height);
VG_API_CALL void vgClear(VGint x, VGint y, VGint width, VGint height);
-}

-- | The standard path format. This is virtually always what you pass to path functions.
{#enum define PathFormat { VG_PATH_FORMAT_STANDARD as PathFormatStandard } deriving (Show, Eq) #}

{#enum VGPathDatatype as PathDatatype {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathAbsRel as PathAbsRel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathSegment as PathSegment {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathCommand as PathCommand {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

newtype Path = Path { pathHandle :: VGHandle }

{#enum VGPathCapabilities as PathCapability {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}
combineCapabilities :: [PathCapability] -> VGbitfield
combineCapabilities = fromIntegral . (foldr ((.|.) . fromEnum) 0)

{#enum VGPathParamType as PathParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGCapStyle as CapStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGJoinStyle as JoinStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}


{#fun vgCreatePath as createPath
    { fromEnum' `PathFormat' -- ^ pathFormat
    , fromEnum' `PathDatatype' -- ^ datatype
    , `Float' -- ^ scale
    , `Float' -- ^ bias
    , `Int' -- ^ segmentCapacityHint
    , `Int' -- ^ coordCapacityHint
    , combineCapabilities `[PathCapability]' -- capabilities
    } -> `Path' Path #}

-- TODO:
{-
VG_API_CALL void vgClearPath(VGPath path, VGbitfield capabilities);
VG_API_CALL void vgDestroyPath(VGPath path);
VG_API_CALL void vgRemovePathCapabilities(VGPath path,
                                          VGbitfield capabilities);
VG_API_CALL VGbitfield vgGetPathCapabilities(VGPath path);
VG_API_CALL void vgAppendPath(VGPath dstPath, VGPath srcPath);
-}
{#fun vgAppendPathData
    { pathHandle `Path' -- ^ dstPath
    , `Int' -- ^ numSegments
    , id `Ptr VGubyte' -- ^ pathSegments
    , id `Ptr ()' -- ^ pathData
    } -> `()' #}
    
appendPathData :: Path -> [(PathCommand, (Float, Float))] -> IO ()
appendPathData p cmds = 
  withArrayLen (map (fromEnum' . fst) cmds) $ \segs ptr1 ->
    withArray (concatMap (\(_,(x,y)) -> [x,y]) cmds) $ \ptr2 ->
      vgAppendPathData p segs ptr1 (castPtr ptr2)

closePath :: Path -> IO ()
closePath p = withArray [fromEnum' ClosePath] $ \ptr -> 
  vgAppendPathData p 1 ptr nullPtr
  
-- TODO:
{-
VG_API_CALL void vgModifyPathCoords(VGPath dstPath, VGint startIndex,
                                    VGint numSegments,
                                    const void * pathData);
VG_API_CALL void vgTransformPath(VGPath dstPath, VGPath srcPath);
VG_API_CALL VGboolean vgInterpolatePath(VGPath dstPath,
                                        VGPath startPath,
                                        VGPath endPath,
                                        VGfloat amount);
VG_API_CALL VGfloat vgPathLength(VGPath path,
                                 VGint startSegment, VGint numSegments);
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
{#fun vgDrawPath as drawPath
    { pathHandle `Path' -- ^ path
    , combinePaintModes `[PaintMode]' -- paintModes
    } -> `()' #} 

------------- Filling, paints and colors -------------

{#enum VGFillRule as FillRule {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPaintMode as PaintMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}
combinePaintModes :: [PaintMode] -> VGbitfield
combinePaintModes = fromIntegral . (foldr ((.|.) . fromEnum) 0)

newtype Paint = Paint { paintHandle :: VGHandle }

{#enum VGPaintParamType as PaintParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPaintType as PaintType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGColorRampSpreadMode as ColorRampSpreadMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgCreatePaint as createPaint { } -> `Paint' Paint #}

-- TODO:
-- VG_API_CALL void vgDestroyPaint(VGPaint paint);

{#fun vgSetPaint as setPaint { paintHandle `Paint', combinePaintModes `[PaintMode]' } -> `()' #}

-- TODO:
{-
VG_API_CALL VGPaint vgGetPaint(VGPaintMode paintMode);
VG_API_CALL void vgSetColor(VGPaint paint, VGuint rgba);
VG_API_CALL VGuint vgGetColor(VGPaint paint);
VG_API_CALL void vgPaintPattern(VGPaint paint, VGImage pattern);
-}

------------- Images -------------

{#enum VGTilingMode as TilingMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGImageFormat as ImageFormat {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

newtype Image = Image VGHandle

{#enum VGImageQuality as ImageQuality {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGImageParamType as ImageParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGImageMode as ImageMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGImageChannel as ImageChannel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGBlendMode as BlendMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO:
{-
VG_API_CALL VGImage vgCreateImage(VGImageFormat format,
                                  VGint width, VGint height,
                                  VGbitfield allowedQuality);
VG_API_CALL void vgDestroyImage(VGImage image);
VG_API_CALL void vgClearImage(VGImage image,
                              VGint x, VGint y, VGint width, VGint height);
VG_API_CALL void vgImageSubData(VGImage image,
                                const void * data, VGint dataStride,
                                VGImageFormat dataFormat,
                                VGint x, VGint y, VGint width, VGint height);
VG_API_CALL void vgGetImageSubData(VGImage image,
                                   void * data, VGint dataStride,
                                   VGImageFormat dataFormat,
                                   VGint x, VGint y,
                                   VGint width, VGint height);
VG_API_CALL VGImage vgChildImage(VGImage parent,
                                 VGint x, VGint y, VGint width, VGint height);
VG_API_CALL VGImage vgGetParent(VGImage image); 
VG_API_CALL void vgCopyImage(VGImage dst, VGint dx, VGint dy,
                             VGImage src, VGint sx, VGint sy,
                             VGint width, VGint height,
                             VGboolean dither);
VG_API_CALL void vgDrawImage(VGImage image);
VG_API_CALL void vgSetPixels(VGint dx, VGint dy,
                             VGImage src, VGint sx, VGint sy,
                             VGint width, VGint height);
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

-- TODO:
-- VG_API_CALL VGHardwareQueryResult vgHardwareQuery(VGHardwareQueryType key,
--                                                   VGint setting);

------------- Renderer and Extension Information -------------

{#enum VGStringID {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- TODO: VG_API_CALL const VGubyte * vgGetString(VGStringID name);

------------- General -------------

{#fun vgFlush {} -> `()' #}

{#fun vgFinish {} -> `()' #}

