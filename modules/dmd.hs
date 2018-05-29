module DMD(
dmd,
initialEnergy
)where


import Numeric.LinearAlgebra

--dmd

customSVD :: Field t => Int -> Matrix t -> (Matrix t, Vector Double, Matrix t)
customSVD dim a = (subU,subS,subV)
  where
  (u,s,v) = compactSVD a
  subU = subMatrix (0,0) (rows u,dim) u
  subS = subVector 0 dim s
  subV = subMatrix (0,0) (rows v,dim) v


-- (v_1 v_2,,,,) という横方向に発展する時系列
dmd :: Matrix C -> (Vector C, Matrix C)
dmd v = (labmda, u <> phi)
  where
  v1 = subMatrix (0,0) ((fst$size v),(snd$size v)-1) v
  v2 = subMatrix (0,1) ((fst$size v),(snd$size v)-1) v
  (u,sigma,w) = customSVD 16 v1 --特異値の打ち切り値、多分
  --(u,sigma,w) = compactSVD v1
  s = (tr u) <> v2 <> w <> (complex (diagRect 0 (1/sigma) (snd$size w) (snd$size u)))::Matrix C
  --print s
  (labmda,phi) = eig s :: (Vector C,Matrix C)

initialEnergy::Matrix C -> Matrix C -> Vector C
initialEnergy v phi = (pinv phi) #> (v #> fromList ([1]++replicate (snd (size v)-1) 0))

--initial_energy::Vector C -> Matrix C -> Vector C
--initial_energy2 v phi = (pinv phi) #> v


check :: Matrix C
check = fromColumns x
  where
    lam = diag (fromList [0.0,0.0,0.95])
    v_1 = fromList [1,0,0]::Vector C
    v_2 = fromList [0,1,0]::Vector C
    v_3 = fromList [0,0,1]::Vector C
    y = fromColumns [v_1,v_2,v_3] : map ( <> lam) y
    x = map (#> (fromList[1,1,1])) (take 10 y)
{-
main = do
  (a,b) <- dmd (check)
  saveMatrix "real.txt" "%lf" (fst$fromComplex check)
  saveMatrix "complex.txt" "%lf" (snd$fromComplex check)
  print (b #> (initial_energy check b))
  --print b
}
