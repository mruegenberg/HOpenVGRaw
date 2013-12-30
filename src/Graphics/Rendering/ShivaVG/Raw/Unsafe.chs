{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw.Unsafe where
-- this module contains a straightforward translation of most of the OpenVG API.
-- don't use this directly, since it doesn't ensure at all that you don't e.g
-- try to set a int parameter with a float

#include <vg/openvg.h>

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}

{#enum VGParamType as ParamType {underscoreToCase} with prefix = "VG_" deriving (Show, Eq)#}

------------- Getting and setting parameters -------------

fromEnum' = fromIntegral . fromEnum

{#fun vgSetf as setf { fromEnum' `ParamType', `Float'} -> `()' #}

{#fun vgSeti as seti { fromEnum' `ParamType', `Int'} -> `()' #}

{#fun vgSetfv { fromEnum' `ParamType', `Int', id `Ptr VGfloat'} -> `()' #}
setfv :: ParamType -> [Float] -> IO ()
setfv t vs = withArrayLen vs' (\len ptr -> vgSetfv t len ptr)
  where vs' = map realToFrac vs
        
{#fun vgSetiv { fromEnum' `ParamType', `Int', id `Ptr VGint'} -> `()' #}
setiv :: ParamType -> [Int] -> IO ()
setiv t vs = withArrayLen vs' (\len ptr -> vgSetiv t len ptr)
  where vs' = map fromIntegral vs
        
{#fun vgGetf as getf { fromEnum' `ParamType'} -> `Float' #}

{#fun vgGeti as geti { fromEnum' `ParamType'} -> `Int' #}

{#fun vgGetVectorSize as getVectorSize { fromEnum' `ParamType'} -> `Int' #}

{#fun vgGetfv { fromEnum' `ParamType', `Int', id `Ptr VGfloat'} -> `()' #}             
getfv :: ParamType -- ^ type
      -> Int -- ^ count
      -> IO [Float] -- ^ values
getfv t c = allocaArray c $ \ptr -> do
  vgGetfv t c ptr
  vs <- peekArray c ptr
  return (map realToFrac vs)
  
{#fun vgGetiv { fromEnum' `ParamType', `Int', id `Ptr VGint'} -> `()' #}             
getiv :: ParamType -- ^ type
      -> Int -- ^ count
      -> IO [Int] -- ^ values
getiv t c = allocaArray c $ \ptr -> do
  vgGetiv t c ptr
  vs <- peekArray c ptr
  return (map fromIntegral vs)
  
{#fun vgSetParameterf as setParameterf { id `VGHandle', `Int', `Float'} -> `()' #}    

{#fun vgSetParameteri as setParameteri { id `VGHandle', `Int', `Int'} -> `()' #}

{#fun vgSetParameterfv { id `VGHandle', fromEnum' `ParamType', `Int', id `Ptr VGfloat'} -> `()' #}    
setParameterfv :: VGHandle -> ParamType -> [Float] -> IO ()
setParameterfv h t vs = withArrayLen vs' (\len ptr -> vgSetParameterfv h t len ptr)
   where vs' = map realToFrac vs

{#fun vgSetParameteriv { id `VGHandle', fromEnum' `ParamType', `Int', id `Ptr VGint'} -> `()' #}
setParameteriv :: VGHandle -> ParamType -> [Int] -> IO ()
setParameteriv h t vs = withArrayLen vs' (\len ptr -> vgSetParameteriv h t len ptr)
   where vs' = map fromIntegral vs
         
{#fun vgGetParameterf as getParameterf { id `VGHandle', fromEnum' `ParamType'} -> `Float' #}

{#fun vgGetParameteri as getParameteri { id `VGHandle', fromEnum' `ParamType'} -> `Int' #}

{#fun vgGetParameterVectorSize as getParameterVectorSize { id `VGHandle', fromEnum' `ParamType'} -> `Int' #}

{#fun vgGetParameterfv { id `VGHandle', fromEnum' `ParamType', `Int', id `Ptr VGfloat'} -> `()' #}             
getParameterfv :: VGHandle -- ^ object
               -> ParamType -- ^ type
               -> Int -- ^ count
               -> IO [Float] -- ^ values
getParameterfv h t c = allocaArray c $ \ptr -> do
  vgGetParameterfv h t c ptr
  vs <- peekArray c ptr
  return (map realToFrac vs)
  
{#fun vgGetParameteriv { id `VGHandle', fromEnum' `ParamType', `Int', id `Ptr VGint'} -> `()' #}             
getParameteriv :: VGHandle -- ^ object
               -> ParamType -- ^ type
               -> Int -- ^ count
               -> IO [Int] -- ^ values
getParameteriv h t c = allocaArray c $ \ptr -> do
  vgGetParameteriv h t c ptr
  vs <- peekArray c ptr
  return (map fromIntegral vs)
  