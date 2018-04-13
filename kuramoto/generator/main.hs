import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple


k = 2.0 :: R--結合強度
dt = 0.01 :: R--結合強度

order ::(Vector R -> C)
order x = sum(fmap (\x->cos x :+ sin x) (toList x))/(fromIntegral (size x))

f::(Vector R -> (Vector R,C) -> (Vector R,C))
f omega (x,o) = (x_2,o_2)
  where
    x_2 =  x + cmap (*dt) omega + cmap (\x -> dt * k * fst(polar o) * sin (snd(polar o)-x)) x
    o_2 = order x_2

main = do
  let x = fromList [0,1,0]::Vector R
  let v = fromList [-1,0,1]::Vector R
  let o = order x
  let g = f v :: ((Vector R,C) -> (Vector R,C))
  let r = fst . polar .snd 
  print$(r .  g) (x,o)

