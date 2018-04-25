module Fourier (
  fourier,
  reconstruct,
  calcErr
)where
import Numeric.LinearAlgebra

fourier::Vector R -> [(R,R)]
fourier f = zip cans sans
  where
  l = (toEnum . size) f
  -- n = 1,2,3..
  numericSin n = (cmap (\x->sin (pi*n*x)) . fromList) [-1,-1+(2/(l-1)) .. 1]::Vector R
  numericCos n = (cmap (\x->cos (pi*n*x)) . fromList) [-1,-1+(2/(l-1)) .. 1]::Vector R
  sans = [2*ans/l|x <- [0..], let ans = f <.> (numericSin x)]
  cans = [2*ans/l|x <- [0..], let ans = f <.> (numericCos x)]

reconstruct ans x = sum [cal|n<-[1..(k-1)],let cal = (c!!n)*cos(pi*toEnum n*x)+(s!!n)*sin(pi*toEnum n*x)] + c!!0 /2
  where
  (c,s) = unzip ans::([R],[R])
  k = (toEnum . length) ans


calcErr::Vector R -> [(R,R)] -> R
calcErr f ans = err
  where
  l = (toEnum . size) f
  i = fromList [-1,-1+(2/(l-1)) .. 1]::Vector R
  err = norm_2 (f - (cmap (reconstruct ans) i)) / (toEnum . size) f


{-
x = fromList [-1,-0.999..1]::Vector R
fy l= sin(pi*l)
fz l= cos(pi*2*l) + 3
y = cmap fy x::Vector R
z = cmap fz x::Vector R
-}
