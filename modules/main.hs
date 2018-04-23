import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple
import Kuramoto
--import Daido
import DMD


main = do
  let n = 1000 :: Int
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

costumPlotPaths z legend filename= plotPathsStyle atribute plotstyle
  where
  fontType = "Helvetica"
  tics     = Custom "tics"   ["font","\""++fontType++",16\""] -- 目盛りのフォントの変更
  xlavel   = Custom "xlabel" ["font","\""++fontType++",20\""] -- xlabelのフォントの変更
  ylavel   = Custom "ylabel" ["font","\""++fontType++",20\""] -- ylabelのフォントの変更
  keyFont  = Custom "key"    ["font","\""++fontType++",18\""] -- 凡例のフォントの変更    
  titleFont= Custom "title"  ["font","\""++fontType++",24\""] -- タイトルのフォントの変更    
  key = [(Key (Just["box lt 8 lw 1"]))]--凡例の枠囲み凡例の位置変更はこのリス
  label = [(YLabel "mode"),(XLabel "\\phi")]--軸の見出し
  save = [EPS filename,Custom "terminal" ["eps","enhanced","color"],Custom "bmargin" ["4"]]
  --save = [PNG "test.png"]　-- PNGで出力するときは上の一文をコメントアウトしこれを使う。
  title = [Title "sin and arcsin"]--グラフのタイトル
  size = [Aspect (Ratio 0.7)]--グラフの形　縦/横
  font = [tics,xlavel,ylavel,keyFont,titleFont]
  atribute = (save++key++label++title++size++font)--fontは後述
  plotstyle = [x|i <-[0..(length z-1)],let x = (defaultStyle {lineSpec = CustomStyle [(LineTitle (legend!!i)),(LineWidth 2.0)]},z!!i)]



plotMode mode n
  |n == 0 = plotList [PNG ("mode/mode"++(show n)++".png")] (toList ((toColumns (fst (fromComplex mode)))!!0) ++  toList ((toColumns (fst (fromComplex mode)))!!0))
  |otherwise = do
    plotList [PNG ("mode/mode"++(show n)++".png")] (toList ((toColumns (fst (fromComplex mode)))!!n) ++  toList ((toColumns (fst (fromComplex mode)))!!n))
    plotMode mode (n-1)


