
module Kuramoto (
  order,
  makeData,
  parMakeData,
  lorentz,
  phaseDensity,
  sindis
)where

{-
振動子数 n は明記されていない。初期値、固有振動数分布からわかる。
ステップ数 l
結合強度 k
刻み幅 dt
を引数にする。
-}

import Numeric.LinearAlgebra
--import Graphics.Gnuplot.Simple
import Control.Parallel.Strategies


modPi :: R -> R
modPi x = snd (polar(cos x :+ sin x)) -- [-pi.pi]に写像

--order parameter の計算
order ::(Vector R -> C)
order x = sum(fmap (\x->cos x :+ sin x) (toList x))/(fromIntegral (size x))

--1 step後の計算
f::((R,R) -> Vector R -> Vector R -> Vector R)
f (k,dt) v x = x_2
  where
    o = order x
    x_2 = cmap modPi (x + cmap (*dt) v + cmap (\x -> dt * k * fst(polar o) * sin (snd(polar o)-x)) x)

parCmap func vec = fromList$parMap rpar func (toList vec)


parF::((R,R) -> Vector R -> Vector R -> Vector R)
parF (k,dt) v x = x_2
  where
    o = order x
    x_2 = parCmap modPi (x + parCmap (*dt) v + parCmap (\x -> dt * k * fst(polar o) * sin (snd(polar o)-x)) x)


--時系列のmatrix生成
makeData::(R,R) -> (Vector R,Vector R) -> Int -> (Matrix R,Vector C)

makeData (k,dt) (v,x_0) l = (fromColumns (take l phi),psi)
  where
    phi = x_0 : map (f (k,dt) v) phi
    psi = fromList (map order (take l phi))

parMakeData (k,dt) (v,x_0) l = (fromColumns (take l phi),psi)
  where
    phi = x_0 : map (parF (k,dt) v) phi
    psi = fromList (parMap rpar order (take l phi))

lorentz::Double -> Int -> [R]
lorentz gamma n = [gamma * tan (pi*x) | p <- [1..n],let x = (toEnum p)/(toEnum n+1)-0.5 :: R]

sindis n = [2*asin(2*x-1) | p <-[1..n], let x = (toEnum p)/(toEnum n+1)::R ]

phaseDensity :: Int -> Matrix R -> Matrix R
phaseDensity m phase = rho
  where
    chi_i::Int -> R -> R
    chi_i i x = if (toEnum i)/(toEnum m) <= (x+pi)/(2.0*pi) && (x+pi)/(2.0*pi) < (toEnum (i+1))/(toEnum m) then 1.0 else 0.0
    chi :: Vector R -> Vector R
    chi x = fromList [y/toEnum (size x) | i <- [0..(m-1)],let y = norm_1 (cmap (chi_i i) x)] -fromList (replicate m (1.0/toEnum m)) :: Vector R
    rho = fromColumns (map chi (toColumns phase)):: Matrix R
{-
main = do
  let n = 10 :: Int
  let v = fromList (lorentz 2.0 n)::Vector R
  let x = fromList (replicate n 0)::Vector R
  let (a,b) = makeData  (1.80,0.005) (v,x) 10:: (Matrix R,Vector C)
  let b_r = map realPart (toList b)
  let b_c = map imagPart (toList b)
  print (size a)
  print (phaseDensity 5 a)
  plotList [PNG "test.png"] b_r
-}



