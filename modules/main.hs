import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
import DMD

main = do
  let n = 100 :: Int
  let v = fromList (lorentz 2.0 n)::Vector R
  let x = fromList (replicate n 0)::Vector R
  let (k,dt)=(1.8,0.005)
  let (a,b) = make_data (k,dt) (v,x) 1000:: (Matrix R,Vector C)
  let b_r = map realPart (toList b)
  let b_c = map imagPart (toList b)
  c <-  (dmd ( toComplex (a,a*0)))
  print c
  plotList [PNG "test.png"] b_r


