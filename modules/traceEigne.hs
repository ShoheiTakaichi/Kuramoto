import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
import Fourier
--import Daido
import DMD
import CustomPlot
import Data.List 
import Control.Parallel.Strategies

--oneOp k = maxElement (cmap (\x-> (log . realPart . abs) x / dt) lam)
oneOp k = ((sort . toList) (cmap (\x-> (log . realPart . abs) x / dt) lam))
  where
  n = 1000 :: Int
  v = fromList (lorentz 1.0 n)::Vector R
  --let x = fromList (linearScale (toInteger n-1) (-pi,pi)) :: Vector R
  x = fromList (replicate n 0)::Vector R
  dt= 0.00050
  h = -0.5
  (a,b) = makeData (k,dt) (v,x) 10000:: (Matrix R,Vector C)
  rho = phaseDensity 100 a
  c = [cmap abs x | i <-[0..7],let x = subVector i (size b-8) b]
  delay = fromRows c
  (lam,mode) = (dmd ( toComplex (rho,rho*0)))::(Vector C,Matrix C)
  --(lam,mode) = (dmd delay)::(Vector C,Matrix C)


main = do
  let ks = [1.6,1.61..2.1]
  let lams = (\x -> map toList ((toRows . fromColumns) (map fromList x))) (parMap rpar oneOp ks)
  --let lams = (map oneOp ks)
  let emb = map (\x -> (x -2.0)/2.0) ks
  --let line1 = map (\x -> zip ks (lams!!x)) [0]
  let line1 = [zip ks (last lams)]
  let line2 = zip ks emb
  let save = [EPS "test.eps",Custom "terminal" ["eps","enhanced","color"],Custom "bmargin" ["4"],Key Nothing]
 
  plotPaths save (line1 ++ [line2])
  --plotPaths [Key Nothing] (line1 ++ [line2])
  --plotList [PNG "eig.png"] lams
 
