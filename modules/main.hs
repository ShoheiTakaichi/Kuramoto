import Numeric.LinearAlgebra
--import Graphics.Gnuplot.Simple
import Kuramoto
import DMD

main = do
  let n = 100 :: Int
  let v = fromList (lorentz 2.0 n)::Vector R
  let x = fromList (replicate n 0)::Vector R
  let (k,dt)=(1.8,0.005)
  let (a,b) = makeData (k,dt) (v,x) 1000:: (Matrix R,Vector C)
  let rho = phaseDensity 10 a
  c <-  (dmd ( toComplex (rho,rho*0)))
  print (fst c)
  --plotList [PNG "test.png"] b_r


