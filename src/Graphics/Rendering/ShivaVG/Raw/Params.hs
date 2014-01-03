{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Rendering.ShivaVG.Raw.Params where

import Graphics.Rendering.ShivaVG.Raw.Unsafe
import Graphics.Rendering.ShivaVG.Raw.Internal
import Data.Bits((.&.))

data Param a = Param (IO a) (a -> IO ())
hEnumParam :: (Enum p, Enum v) => VGHandle -> p -> Param v
hEnumParam h p = Param (mToEnum (getParameteri h p)) (\v -> setParameteri h p (fromEnum v))

enumParam :: (Enum p, Enum v) => p -> Param v
enumParam p = Param (mToEnum (geti p)) (\v -> seti p (fromEnum v))

hFVParam :: (Enum p) => VGHandle -> p -> (a -> [Float]) -> ([Float] -> a) -> Param a
hFVParam h p tofv fromfv = Param getter (\v -> setParameterfv h p (tofv v))
  where
    getter = do
      s <- getVectorSize p
      vs <- getParameterfv h p s
      return (fromfv vs)
      
fvParam :: (Enum p) => p -> (a -> [Float]) -> ([Float] -> a) -> Param a
fvParam p tofv fromfv = Param getter (\v -> setfv p (tofv v))
  where
    getter = do
      s <- getVectorSize p
      vs <- getfv p s
      return (fromfv vs)
      
hBoolParam :: (Enum p) => VGHandle -> p -> Param Bool
hBoolParam h p = Param 
                (mVGBool2Bool $ mToEnum (getParameteri h p)) 
                (\v -> setParameteri h p (fromEnum (bool2vgBool v)))
                
boolParam :: (Enum p) => p -> Param Bool
boolParam p = Param 
              (mVGBool2Bool $ mToEnum (geti p)) 
              (\v -> seti p (fromEnum (bool2vgBool v)))
                
hFloatParam :: (Enum p) => VGHandle -> p -> Param Float
hFloatParam h p = Param
                 (getParameterf h p)
                 (setParameterf h p)
                 
floatParam :: (Enum p) => p -> Param Float
floatParam p = Param
               (getf p)
               (setf p)
                 
combinedOptsParam :: (Enum p, Enum a) => p -> [a] -> Param [a]
combinedOptsParam p allOpts = Param
                              ((geti p) >>= \i -> return $ filter ((/= 0) . (.&. i) . fromIntegral . fromEnum) allOpts)
                              (seti p . fromIntegral . combineOptions)

get :: Param a -> IO a
get (Param g _) = g

set :: (Param a) -> a -> IO ()
set (Param _ s) v = s v
