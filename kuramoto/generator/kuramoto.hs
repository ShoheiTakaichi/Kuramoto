module Kuramoto (
  order,
  make_data,
  lorentz
)where
{-
振動子数 n は明記されていない。初期値、固有振動数分布からわかる。
ステップ数 l
結合強度 k
刻み幅 dt
を引数にする。
-}

import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple




--order parameter の計算
order ::(Vector R -> C)
order x = sum(fmap (\x->cos x :+ sin x) (toList x))/(fromIntegral (size x))

--1 step後の計算
f::((R,R) -> Vector R -> Vector R -> Vector R)
f (k,dt) v x = x_2
  where
    o = order x
    x_2 =  x + cmap (*dt) v + cmap (\x -> dt * k * fst(polar o) * sin (snd(polar o)-x)) x


--時系列のmatrix生成
make_data::(R,R) -> (Vector R,Vector R) -> Int -> (Matrix R,Vector C)

make_data (k,dt) (v,x_0) l = (fromRows (take l phi),psi)
  where
    phi = x_0 : map (f (k,dt) v) phi
    psi = fromList (map order (take l phi))


lorentz::Double -> Int -> [R]
lorentz gamma n = [gamma * tan (pi*x) | p <- [1..n],let x = (toEnum p)/(toEnum n+1)-0.5 :: R]
  
{-
main = do
  let n = 1000 :: Int
  let v = fromList (lorentz 2.0 n)::Vector R
  let x = fromList (replicate n 0)::Vector R
  let (a,b) = make_data (v,x) 10000:: (Matrix R,Vector C)
  let b_r = map realPart (toList b)
  let b_c = map imagPart (toList b)
  print (size b)
  plotList [PNG "test.png"] b_r
-}
