# HOpenVGRaw

The aim of this project is to provide relatively low level bindings to [OpenVG](http://www.khronos.org/openvg/), specifically the [ShivaVG](http://sourceforge.net/projects/shivavg/) implementation. 
(For the most part, the official OpenVG 1.0.1 headers are used, so adapting the library for other OpenVG implementation should be straightforward.)

These bindings are intended to be simpler, safer, use Haskell datatypes where possible and be easier to compile than the [existing bindings](http://hackage.haskell.org/package/OpenVGRaw). 

## How to use
1. Install ShivaVG to where it can be found (e.g `/usr/local`). <br>
If you are on a Unix-like system, chances are that your package manager supplies it, e.g on OS X with Homebrew, simply do `brew install shivavg`.

2. Install this package like you would any other Cabal package

## Examples
The `test` directory contains a simple test program in both Haskell, using these bindings and C.

## License
Like many Haskell packages, this code is licensed under the BSD3 license. 

ShivaVG itself, written by Ivan Leben, is licensed under the LGPL