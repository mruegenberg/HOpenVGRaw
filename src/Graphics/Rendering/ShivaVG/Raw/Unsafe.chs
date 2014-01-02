{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw.Unsafe where
-- this module contains a straightforward translation of unsafe parts of the OpenVG API.
-- don't use this directly, since it doesn't ensure at all that you don't e.g
-- try to set a int parameter with a float

#include <vg/openvg.h>

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}

{#enum VGParamType as ParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Getting and setting parameters -------------

fromEnum' :: (Enum p) => p -> CInt
fromEnum' = fromIntegral . fromEnum

-- instead of the obvious choice of `ParamType` as the argument of what to set, we use Enum,
-- since different param types can be used (e.g ParamType and PaintParamType)

{#fun vgSetf as setf `(Enum p)' => { fromEnum' `p', `Float'} -> `()' #}

{#fun vgSeti as seti `(Enum p)' => { fromEnum' `p', `Int'} -> `()' #}

{#fun vgSetfv `(Enum p)' => { fromEnum' `p', `Int', id `Ptr VGfloat'} -> `()' #}
setfv :: (Enum p) => p -> [Float] -> IO ()
setfv t vs = withArrayLen vs' (\len ptr -> vgSetfv t len ptr)
  where vs' = map realToFrac vs
        
{#fun vgSetiv `(Enum p)' => { fromEnum' `p', `Int', id `Ptr VGint'} -> `()' #}
setiv :: (Enum p) => p -> [Int] -> IO ()
setiv t vs = withArrayLen vs' (\len ptr -> vgSetiv t len ptr)
  where vs' = map fromIntegral vs
        
{#fun vgGetf as getf `(Enum p)' => { fromEnum' `p' } -> `Float' #}

{#fun vgGeti as geti `(Enum p)' => { fromEnum' `p' } -> `Int' #}

{#fun vgGetVectorSize as getVectorSize `(Enum p)' => { fromEnum' `p' } -> `Int' #}

{#fun vgGetfv `(Enum p)' => { fromEnum' `p', `Int', id `Ptr VGfloat'} -> `()' #}             
getfv :: (Enum p)
      => p -- ^ type
      -> Int -- ^ count
      -> IO [Float] -- ^ values
getfv t c = allocaArray c $ \ptr -> do
  vgGetfv t c ptr
  vs <- peekArray c ptr
  return (map realToFrac vs)
  
{#fun vgGetiv `(Enum p)' => { fromEnum' `p', `Int', id `Ptr VGint'} -> `()' #}             
getiv :: (Enum p)
      => p -- ^ type
      -> Int -- ^ count
      -> IO [Int] -- ^ values
getiv t c = allocaArray c $ \ptr -> do
  vgGetiv t c ptr
  vs <- peekArray c ptr
  return (map fromIntegral vs)
  
{#fun vgSetParameterf as setParameterf { id `VGHandle', `Int', `Float'} -> `()' #}    

{#fun vgSetParameteri as setParameteri { id `VGHandle', `Int', `Int'} -> `()' #}

{#fun vgSetParameterfv `(Enum p)' => { id `VGHandle', fromEnum' `p', `Int', id `Ptr VGfloat'} -> `()' #}    
setParameterfv :: (Enum p) => VGHandle -> p -> [Float] -> IO ()
setParameterfv h t vs = withArrayLen vs' (\len ptr -> vgSetParameterfv h t len ptr)
   where vs' = map realToFrac vs

{#fun vgSetParameteriv `(Enum p)' => { id `VGHandle', fromEnum' `p', `Int', id `Ptr VGint'} -> `()' #}
setParameteriv :: (Enum p) => VGHandle -> p -> [Int] -> IO ()
setParameteriv h t vs = withArrayLen vs' (\len ptr -> vgSetParameteriv h t len ptr)
   where vs' = map fromIntegral vs
         
{#fun vgGetParameterf as getParameterf `(Enum p)' => { id `VGHandle', fromEnum' `p'} -> `Float' #}

{#fun vgGetParameteri as getParameteri `(Enum p)' => { id `VGHandle', fromEnum' `p'} -> `Int' #}

{#fun vgGetParameterVectorSize as getParameterVectorSize `(Enum p)' => { id `VGHandle', fromEnum' `p'} -> `Int' #}

{#fun vgGetParameterfv `(Enum p)' => { id `VGHandle', fromEnum' `p', `Int', id `Ptr VGfloat'} -> `()' #}             
getParameterfv :: (Enum p)
               => VGHandle -- ^ object
               -> p -- ^ type
               -> Int -- ^ count
               -> IO [Float] -- ^ values
getParameterfv h t c = allocaArray c $ \ptr -> do
  vgGetParameterfv h t c ptr
  vs <- peekArray c ptr
  return (map realToFrac vs)
  
{#fun vgGetParameteriv `(Enum p)' => { id `VGHandle', fromEnum' `p', `Int', id `Ptr VGint'} -> `()' #}             
getParameteriv :: (Enum p)
               => VGHandle -- ^ object
               -> p -- ^ type
               -> Int -- ^ count
               -> IO [Int] -- ^ values
getParameteriv h t c = allocaArray c $ \ptr -> do
  vgGetParameteriv h t c ptr
  vs <- peekArray c ptr
  return (map fromIntegral vs)
  