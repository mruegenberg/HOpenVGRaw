{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.ShivaVG.Raw
       ( 
         -- * ShivaVG extensions
         createContextSH
       , resizeSurfaceSH
       , destroyContextSH
         -- * Modules
       , module Graphics.Rendering.ShivaVG.Raw.Types
       )
       where

#include <vg/openvg.h>

import Foreign.C.Types
{#import Graphics.Rendering.ShivaVG.Raw.Internal#}
{#import Graphics.Rendering.ShivaVG.Raw.Types#}

---------------- ShivaVG extensions ----------------
-- {{{

int2Bool = toEnum . fromEnum

{#fun vgCreateContextSH as createContextSH
     { `Int',
       `Int'
     } -> `Bool' int2Bool #}

{#fun vgResizeSurfaceSH as resizeSurfaceSH { `Int', `Int' } -> `()' #}

{#fun vgDestroyContextSH as destroyContextSH {} -> `()' #}

-- }}}