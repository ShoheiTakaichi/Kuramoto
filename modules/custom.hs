
module CustomPlot (
  plotMode,
  plotOrder
)where

import Graphics.Gnuplot.Simple
import Numeric.LinearAlgebra

--z::[[(Double,Double)]] legend::[String]
phaseDensePlot z titleName file = plotPath atribute z
  where
  fontType = "Helvetica"
  tics     = Custom "tics"   ["font","\""++fontType++",16\""] -- 目盛りのフォントの変更
  xlavel   = Custom "xlabel" ["font","\""++fontType++",20\""] -- xlabelのフォントの変更
  ylavel   = Custom "ylabel" ["font","\""++fontType++",20\""] -- ylabelのフォントの変更
  keyFont  = Custom "key"    ["font","\""++fontType++",18\""] -- 凡例のフォントの変更    
  titleFont= Custom "title"  ["font","\""++fontType++",24\""] -- タイトルのフォントの変更    
  label = [(YLabel "mode"),(XLabel "phase")]--軸の見出し
  --save = [EPS (file++".eps"),Custom "terminal" ["eps","enhanced","color"],Custom "bmargin" ["4"]]
  save = [PNG (file++".png")]　-- PNGで出力するときは上の一文をコメントアウトしこれを使う。
  --size = [Aspect (Ratio 10)]--グラフの形　縦/横
  size = []
  font = [tics,xlavel,ylavel,keyFont,titleFont]
  xformat = [XFormat "%.1P{/Symbol p}"]
  xticks = [XTicks (Just["0.5*pi"])]++[YTicks Nothing]
  title = [Title titleName]
  key = [Key Nothing]
  atribute = (save++label++title++size++font++xformat++key)--fontは後述

 
costumPlotPaths z legend = plotPathsStyle atribute plotstyle
  where
  fontType = "Helvetica"
  tics     = Custom "tics"   ["font","\""++fontType++",16\""] -- 目盛りのフォントの変更
  xlavel   = Custom "xlabel" ["font","\""++fontType++",20\""] -- xlabelのフォントの変更
  ylavel   = Custom "ylabel" ["font","\""++fontType++",20\""] -- ylabelのフォントの変更
  keyFont  = Custom "key"    ["font","\""++fontType++",18\""] -- 凡例のフォントの変更    
  titleFont= Custom "title"  ["font","\""++fontType++",24\""] -- タイトルのフォントの変更    
  key = [(Key (Just["box lt 8 lw 1"]))]--凡例の枠囲み凡例の位置変更はこのリストに(Key (Just["right bottom"]))等を追加
  xticks = [XTicks (Just["0.5*pi"])]
  label = [(YLabel "y"),(XLabel "x")]--軸の見出し
  save = [EPS "test.eps",Custom "terminal" ["eps","enhanced","color"],Custom "bmargin" ["4"]]
  --save = [PNG "test.png"]　-- PNGで出力するときは上の一文をコメントアウトしこれを使う。
  title = [Title "sin and arcsin"]--グラフのタイトル
  size = [Aspect (Ratio 0.7)]--グラフの形　縦/横
  font = [tics,xlavel,ylavel,keyFont,titleFont]
  xformat = [XFormat "%.1P{/Symbol p}"]
  atribute = (save++key++label++title++size++font++xticks++xformat)--fontは後述
  plotstyle = [x|i <-[0..(length z-1)],let x = (defaultStyle {lineSpec = CustomStyle [(LineTitle (legend!!i)),(LineWidth 2.0)]},z!!i)]


 --例
plotMode::(Matrix C -> Vector C -> Int -> IO())
plotMode mode l n
  |n==0 = do
    let rho = toList ((toColumns (fst (fromComplex mode)))!!n)++toList ((toColumns (fst (fromComplex mode)))!!n)
    let phi = linearScale (toInteger(length rho)-1) (-2*pi,2*pi)
    let z = zip phi rho
    let title = "{/Symbol l}"++(show n)
    let file = "mode/mode"++(show n)
    phaseDensePlot z title file
  |otherwise = do
    let rho = toList ((toColumns (fst (fromComplex mode)))!!n)++toList ((toColumns (fst (fromComplex mode)))!!n)
    let phi = linearScale (toInteger(length rho)-1) (-2*pi,2*pi)
    let z = zip phi rho
    let title = "{/Symbol l}="++(show n)
    let file = "mode/mode"++(show n)
    phaseDensePlot z title file
    plotMode mode l (n-1)

plotOrder::Vector C -> R -> IO()
plotOrder r dt = do
  let t = linearScale (toInteger (size r)-1) (0,dt*(toEnum (size r -1))) ::[R]
  let z = zip t (toList (fst (fromComplex (cmap abs r))))::[(R,R)]
  let fontType = "Helvetica"
  let tics     = Custom "tics"   ["font","\""++fontType++",16\""] -- 目盛りのフォントの変更
  let xlavel   = Custom "xlabel" ["font","\""++fontType++",20\""] -- xlabelのフォントの変更
  let ylavel   = Custom "ylabel" ["font","\""++fontType++",20\""] -- ylabelのフォントの変更
  let keyFont  = Custom "key"    ["font","\""++fontType++",18\""] -- 凡例のフォントの変更    
  let titleFont= Custom "title"  ["font","\""++fontType++",24\""] -- タイトルのフォントの変更    
  let key = [Key Nothing]--凡例の枠囲み凡例の位置変更はこのリストに(Key (Just["right bottom"]))等を追加
  let label = [(YLabel "r"),(XLabel "t")]--軸の見出し
  --save = [EPS "order.eps",Custom "terminal" ["eps","enhanced","color"],Custom "bmargin" ["4"]]
  let save = [PNG "order.png"]　-- PNGで出力するときは上の一文をコメントアウトしこれを使う。
  let title = [Title "order parameter"]--グラフのタイトル
  --size = [Aspect (Ratio 0.7)]--グラフの形　縦/横
  let font = [tics,xlavel,ylavel,keyFont,titleFont]
  --xformat = [XFormat "%.1P{/Symbol p}"]
  let attribute = (save++key++label++title++font++[YRange (0,1)])--fontは後述
  plotPath attribute z
 



