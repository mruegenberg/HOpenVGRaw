{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}
-- | Paths. Corresponds to section 8 of the OpenVG specification.
module Graphics.Rendering.ShivaVG.Raw.Paths
       (
         -- * The Standard Path Format
         PathFormat(..)
       , pathFormat
       , PathDatatype(..)
       , pathDatatype
       , AbsRel(..)
       , PathCommand(..)
         -- * Path Operations
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
       , appendPathDataAbs
       , appendPathDataRel
       , closePath
       , modifyPathCoords
       , pathScale
       , pathBias
       , pathNumSegments
       , transformPath
       , interpolatePath
       -- not implemented in ShivaVG:
       -- , pathLength
       -- , pointAlongPath
       , pathBounds
       , pathTransformedBounds
         -- * Interpretation of Paths
         -- ** Stroke Parameters
       , strokeLineWidth
       , CapStyle(..)
       , strokeCapStyle
       , JoinStyle(..)
       , strokeJoinStyle
       , strokeMiterLimit
       , strokeDashPhase
         -- ** Filling
       , FillRule(..)
       , fillRule
       , PaintMode(..)
         -- ** Drawing
       , drawPath
       ) where

#include <vg/openvg.h>

import Foreign hiding (rotate)
import Foreign.C.Types
import Graphics.Rendering.ShivaVG.Raw.Params
import Graphics.Rendering.ShivaVG.Raw.Internal
import Graphics.Rendering.ShivaVG.Raw.Unsafe

{#enum VGPathParamType as PathParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | The standard path format. This is virtually always what you pass to path functions.
{#enum define PathFormat { VG_PATH_FORMAT_STANDARD as PathFormatStandard } deriving (Show, Eq) #}

-- | Query the command format of the path
pathFormat :: Path -> IO PathFormat
pathFormat (Path p) = mToEnum (getParameteri p PathFormat)

-- | Possible numerical datatypes for path coordinate data.
{#enum VGPathDatatype as PathDatatype {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Query the coordinate datatype of the path
pathDatatype :: Path -> IO PathFormat
pathDatatype (Path p) = mToEnum (getParameteri p PathFormat)  

{#enum VGPathAbsRel as AbsRel {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#enum VGPathSegment {underscoreToCase} deriving (Show, Eq)#}

-- Path segment commands, in absolute and relative variants
-- {#enum VGPathCommand {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

-- | Path segment commands. Refer to section 8 of the OpenVG specification for details
data PathCommand = MoveTo Float Float -- ^ move to x,y
                 | LineTo Float Float -- ^ line to x,y
                 | HLineTo Float -- ^ horizontal line to x
                 | VLineTo Float -- ^ vertical line to y
                 | QuadTo Float Float Float Float -- ^ quadratic segment x0,y0,x1,y1
                 | CubicTo Float Float Float Float Float Float -- ^ cubic segment x0,y0,x1,y1,x2,y2
                 | SQuadTo Float Float -- ^ G1 smooth quad x1,y1
                 | SCubicTo Float Float Float Float -- ^ G1 smooth cubic x1,y1,x2,y2
                 | SCCWArcTo Float Float Float Float Float -- ^ small CCW arc to rh,rv,rot,x0,y0
                 | SCWArcTo Float Float Float Float Float -- ^ small CW arc to rh,rv,rot,x0,y0
                 | LCCWArcTo Float Float Float Float Float -- ^ large CCW arc to rh,rv,rot,x0,y0
                 | LCWArcTo Float Float Float Float Float -- ^ large CW arc to rh,rv,rot,x0,y0
                   
toVGCommand :: PathCommand -> VGPathSegment
toVGCommand (MoveTo _ _) = VgMoveTo
toVGCommand (LineTo _ _) = VgLineTo
toVGCommand (HLineTo _)  = VgHlineTo
toVGCommand (VLineTo _)  = VgVlineTo
toVGCommand (QuadTo _ _ _ _)      = VgQuadTo 
toVGCommand (CubicTo _ _ _ _ _ _) = VgCubicTo
toVGCommand (SQuadTo _ _)      = VgSquadTo
toVGCommand (SCubicTo _ _ _ _) = VgScubicTo
toVGCommand (SCCWArcTo _ _ _ _ _) = VgSccwarcTo
toVGCommand (SCWArcTo _ _ _ _ _)  = VgScwarcTo
toVGCommand (LCCWArcTo _ _ _ _ _) = VgLccwarcTo
toVGCommand (LCWArcTo _ _ _ _ _)  = VgLcwarcTo

toCoords :: PathCommand -> [Float]
toCoords (MoveTo c1 c2) = [c1,c2]
toCoords (LineTo c1 c2) = [c1,c2]
toCoords (HLineTo c1) = [c1]
toCoords (VLineTo c1) = [c1]
toCoords (QuadTo c1 c2 c3 c4) = [c1,c2,c3,c4]
toCoords (CubicTo c1 c2 c3 c4 c5 c6) = [c1,c2,c3,c4,c5,c6]
toCoords (SQuadTo c1 c2) = [c1,c2]
toCoords (SCubicTo c1 c2 c3 c4) = [c1,c2,c3,c4]
toCoords (SCCWArcTo c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]
toCoords (SCWArcTo c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]
toCoords (LCCWArcTo c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]
toCoords (LCWArcTo c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]

-- | Path Handle
newtype Path = Path { pathHandle :: VGHandle }

-- | Set of constants specifying which operations may be performed on a given path object. 
-- 
-- At the time a path is defined, the application specifies which operations it wishes to be able to perform on the path. Over time, the application may disable previously enabled capabilities, but it may not re-enable capabilities once they have been disabled. This feature allows OpenVG implementations to make use of internal path representations that may not support all path operations, possibly resulting in higher performance on paths where those operations will not be performed.
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

-- | Query the scale factor of the path
pathScale :: Path -> IO Float
pathScale (Path p) = getParameterf p PathScale

-- | Query the bias of the path
pathBias :: Path -> IO Float
pathBias (Path p) = getParameterf p PathScale

-- | number of segments stored in the path
pathNumSegments :: Path -> IO Int
pathNumSegments (Path p) = getParameteri p PathNumSegments

-- | total number of specified coordinates stored in the path
pathNumCoords :: Path -> IO Int
pathNumCoords (Path p) = getParameteri p PathNumCoords

-- more elegant way to do this?
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
appendPathData :: Path -> [(PathCommand,AbsRel)] -> IO ()
appendPathData p cmds = 
  withArrayLen (map (\(cmd,absRel) -> (fromEnum' (toVGCommand cmd)) .|. (fromEnum' absRel)) cmds) $ \segs ptr1 ->
    withArray (concatMap (toCoords . fst) cmds) $ \ptr2 ->
      vgAppendPathData p segs ptr1 (castPtr ptr2)

-- | Append absolute path data segments
appendPathDataAbs :: Path -> [PathCommand] -> IO ()
appendPathDataAbs p cmds = appendPathData p (map (\c -> (c,Absolute)) cmds)

-- | Append relative path data segments
appendPathDataRel :: Path -> [PathCommand] -> IO ()
appendPathDataRel p cmds = appendPathData p (map (\c -> (c,Relative)) cmds)

-- | Close the path.
closePath :: Path -> IO ()
closePath p = withArray [fromEnum' VgClosePath] $ \ptr -> 
  vgAppendPathData p 1 ptr nullPtr
  
{#fun vgModifyPathCoords
    { pathHandle `Path' -- ^ dstPath
    , `Int' -- ^ startIndex
    , `Int' -- ^ numSegments
    , id `Ptr ()' -- ^ pathData
    } -> `()' #}
    
-- | Modifies the coordinate data for a contiguous range of segments of dstPath, starting at startIndex (where 0 is the index of the first path segment)
modifyPathCoords :: Path -- ^ dstPath
                 -> Int -- ^ startIndex
                 -> [PathCommand]  -- ^ pathData
                 -> IO ()
modifyPathCoords p startIndex cmds = 
  withArrayLen (concatMap toCoords cmds) $ \segs ptr ->
    vgModifyPathCoords p startIndex segs (castPtr ptr)

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

peekFloat :: Ptr CFloat -> IO Float
peekFloat ptr = peek ptr >>= (return . realToFrac)

-- not implemented in ShivaVG:
{-
-- | Returns the length of a given portion of a path in the user coordinate system (that is, in the path’s own coordinate system, disregarding any matrix settings). Only the subpath consisting of the numSegments path segments beginning with startSegment (where the initial path segment has index 0) is used. If an error occurs, -1.0f is returned.
-- The `PathCapabilityPathLength` capability must be enabled for path.
{#fun vgPathLength as pathLength
    { pathHandle `Path' -- ^ path
    , `Int' -- ^ startSegment
    , `Int' -- ^ numSegments
    } -> `Float' #}

-- | returns the point lying a given distance along a given portion of a path and the unit-length tangent vector at that point. Only the subpath consisting of the numSegments path segments beginning with startSegment (where the initial path segment has index 0) is used. 
{#fun vgPointAlongPath as pointAlongPath
    { pathHandle `Path' -- ^ path
    , `Int' -- ^ startSegment
    , `Int' -- ^ numSegments
    , `Float' -- ^ distance
    , alloca- `Float' peekFloat* -- ^ x
    , alloca- `Float' peekFloat* -- ^ y
    , alloca- `Float' peekFloat* -- ^ tangentX
    , alloca- `Float' peekFloat* -- ^ tangentY
    } -> `()' #}
-}

-- | Returns an axis-aligned bounding box that tightly bounds the interior of the given path. Stroking parameters are ignored.
{#fun vgPathBounds as pathBounds
    { pathHandle `Path' -- ^ path
    , alloca- `Float' peekFloat* -- ^ minX
    , alloca- `Float' peekFloat* -- ^ minY
    , alloca- `Float' peekFloat* -- ^ width
    , alloca- `Float' peekFloat* -- ^ height
    } -> `()' #}

-- | Returns an axis-aligned bounding box that is guaranteed to enclose the geometry of the given path following transformation by the current path-user-to-surface transform. The returned bounding box is not guaranteed to fit tightly around the path geometry.
{#fun vgPathTransformedBounds as pathTransformedBounds
    { pathHandle `Path' -- ^ path
    , alloca- `Float' peekFloat* -- ^ minX
    , alloca- `Float' peekFloat* -- ^ minY
    , alloca- `Float' peekFloat* -- ^ width
    , alloca- `Float' peekFloat* -- ^ height
    } -> `()' #}

strokeLineWidth :: Param Float
strokeLineWidth = floatParam StrokeLineWidth

-- | Defines constants for the Butt, Round, and Square end cap styles
{#enum VGCapStyle as CapStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

strokeCapStyle :: Param CapStyle
strokeCapStyle = enumParam StrokeCapStyle

-- | Defines constants for the Miter, Round, and Bevel line join styles
{#enum VGJoinStyle as JoinStyle {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

strokeJoinStyle :: Param JoinStyle
strokeJoinStyle = enumParam StrokeJoinStyle

strokeMiterLimit :: Param Float
strokeMiterLimit = floatParam StrokeMiterLimit

strokeDashPhase :: Param Float
strokeDashPhase = floatParam StrokeDashPhase

strokeDashPhaseReset :: Param Bool
strokeDashPhaseReset = boolParam StrokeDashPhaseReset

-- | Constants for the even/odd and non- zero fill rules.
{#enum VGFillRule as FillRule {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

fillRule :: Param FillRule
fillRule = enumParam FillRule

-- | Constants for stroking and filling paths, to be used by the `drawPath`, `setPaint`, and `getPaint` functions.
{#enum VGPaintMode as PaintMode {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

{#fun vgDrawPath as drawPath
    { pathHandle `Path' -- ^ path
    , combineOptions `[PaintMode]' -- paintModes
    } -> `()' #} 
