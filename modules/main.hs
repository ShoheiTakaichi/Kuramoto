import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
--import Daido
import DMD





plotMode mode n
  |n == 0 = plotList [PNG ("mode/mode"++(show n)++".png")] (toList ((toColumns (fst (fromComplex mode)))!!0) ++  toList ((toColumns (fst (fromComplex mode)))!!0))
  |otherwise = do
    plotList [PNG ("mode/mode"++(show n)++".png")] (toList ((toColumns (fst (fromComplex mode)))!!n) ++  toList ((toColumns (fst (fromComplex mode)))!!n))
    plotMode mode (n-1)



main = do
  let n = 10000 :: Int
  let v = fromList (lorentz 2.0 n)::Vector R
  let x = fromList (replicate n 0)::Vector R
  let (k,dt)=(1.8,0.005)
  let (a,b) = makeData (k,dt) (v,x) 10000:: (Matrix R,Vector C)
  let rho = phaseDensity 100 a
  let (lam,mode) = (dmd ( toComplex (rho,rho*0)))::(Vector C,Matrix C)
  plotMode mode (size lam -1)
  --plotList [PNG "test.png"] (toList (fst (fromComplex lam)))
  print$size lam
  print ((toList lam)!!2)


