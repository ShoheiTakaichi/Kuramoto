import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
import Fourier
--import Daido
import DMD
import CustomPlot

main = do
  let n = 1000 :: Int
  let v = fromList (lorentz 1.0 n)::Vector R
  --let x = fromList (linearScale (toInteger n-1) (-pi,pi)) :: Vector R
  let x = fromList (replicate n 0)::Vector R
  let (k,dt)=(1.80,0.00050)
  let (a,b) = makeData (k,dt) (v,x) 10000:: (Matrix R,Vector C)
  let rho = phaseDensity 100 a
  let (lam,mode) = (dmd ( toComplex (rho,rho*0)))::(Vector C,Matrix C)
  plotMode mode (cmap (\x->log x / dt) (fst (fromComplex lam))) (size lam -1)
  plotOrder b dt
  --plotList [PNG "test.png"] (toList (fst (fromComplex lam)))
  print$size lam
  print (log ((toList (fst(fromComplex lam)))!!0) / dt)
  g (fst (fromComplex mode)) (size lam-1)


--f = map ((take 100) . fourier) . toColumns
--g :: Matrix R->Int-> IO()
g x n 
  |n > 0 = do
    let y = toColumns x
    let (c,s) = unzip (((take 100) . fourier) (y!!n)) ::([R],[R])
    let z = fromColumns [fromList c,fromList s]
    saveMatrix ("mode/fourier/"++show n++".txt") "%lf" z
    g x (n-1)
  |n==0 = do
    let y = toColumns x
    let (c,s) = unzip (((take 100) . fourier) (y!!n)) ::([R],[R])
    let z = fromColumns [fromList c,fromList s]
    saveMatrix ("mode/fourier/"++show n++".txt") "%lf" z

