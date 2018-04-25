import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
--import Daido
import DMD
import CustomPlot

main = do
  let n = 10000 :: Int
  let v = fromList (lorentz 1.0 n)::Vector R
  --let x = fromList (linearScale (toInteger n-1) (-pi,pi)) :: Vector R
  let x = fromList (replicate n 0)::Vector R
  let (k,dt)=(1.80,0.0005)
  let (a,b) = makeData (k,dt) (v,x) 10000:: (Matrix R,Vector C)
  let rho = phaseDensity 500 a
  let (lam,mode) = (dmd ( toComplex (rho,rho*0)))::(Vector C,Matrix C)
  plotMode mode (cmap (\x->log x / dt) (fst (fromComplex lam))) (size lam -1)
  plotOrder b 0.005
  --plotList [PNG "test.png"] (toList (fst (fromComplex lam)))
  print$size lam
  print (log ((toList (fst(fromComplex lam)))!!0) / dt)

