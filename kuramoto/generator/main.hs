import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple


k = 2.0 :: R--結合強度
dt = 0.01 :: R--結合強度


--order parameter の計算
order ::(Vector R -> C)
order x = sum(fmap (\x->cos x :+ sin x) (toList x))/(fromIntegral (size x))

--1 step後の計算
f::(Vector R -> Vector R -> Vector R)
f v x = x_2
  where
    o = order x
    x_2 =  x + cmap (*dt) v + cmap (\x -> dt * k * fst(polar o) * sin (snd(polar o)-x)) x


--時系列のmatrix生成
make_data::(Vector R,Vector R) -> Int -> (Matrix R,Vector C)

make_data (v,x_0) n = (fromRows (take n phi),psi)
  where
    phi = x_0 : map (f v) phi
    psi = fromList ( map order (take n phi))
    

main = do
  let x = fromList [0,1,0]::Vector R
  let v = fromList [-1,0,1]::Vector R
  let (a,b) = make_data (v,x) 10:: (Matrix R,Vector C)
  print a
