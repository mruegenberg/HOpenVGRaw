{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Rendering.ShivaVG.Raw.Params where

import Graphics.Rendering.ShivaVG.Raw.Unsafe
import Graphics.Rendering.ShivaVG.Raw.Internal


data Param a = Param (IO a) (a -> IO ())
enumParam :: (Enum p, Enum v) => VGHandle -> p -> Param v
enumParam h p = Param (mToEnum (getParameteri h p)) (\v -> setParameteri h p (fromEnum v))

fvParam :: (Enum p) => VGHandle -> p -> (a -> [Float]) -> ([Float] -> a) -> Param a
fvParam h p tofv fromfv = Param getter (\v -> setParameterfv h p (tofv v))
  where
    getter = do
      s <- getVectorSize p
      vs <- getParameterfv h p s
      return (fromfv vs)
      
boolParam :: (Enum p) => VGHandle -> p -> Param Bool
boolParam h p = Param 
                (mVGBool2Bool $ mToEnum (getParameteri h p)) 
                (\v -> setParameteri h p (fromEnum (bool2vgBool v)))

get :: Param a -> IO a
get (Param g _) = g

set :: (Param a) -> a -> IO ()
set (Param _ s) v = s v
