--import Kuramoto
import Daido
import CustomPlot
import Numeric.LinearAlgebra
import Control.Parallel.Strategies
import Graphics.Gnuplot.Simple

bif :: R -> R
bif k = y
  where
  n = 1000 :: Int
  v = fromList (lorentz 1.0 n)::Vector R
  --x = fromList (linearScale (toInteger n-1) (-pi,pi)) :: Vector R
  x = fromList (replicate n 0)::Vector R
  dt=0.0020
  (a,b) = parMakeData (k,-0.5,dt) (v,x) 20000:: (Matrix R,Vector C)
  y = (realPart . abs) ((toList b)!!(size b -1))


main = do
  let k = [1.0,1.1..3.0]
  let ys = parMap rpar bif k
  let z = zip k ys
  plotPath [PNG "bif.png"] z

