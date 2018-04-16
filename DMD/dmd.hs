import Numeric.LinearAlgebra

--dmd


-- (v_1 v_2,,,,) という横方向に発展する時系列
dmd :: Matrix C -> IO ((Vector C, Matrix C))
dmd v = do
  let v1 = subMatrix (0,0) ((fst$size v),(snd$size v)-1) v
  let v2 = subMatrix (0,1) ((fst$size v),(snd$size v)-1) v
  let (u,sigma,w) = svd v1
  let s = (tr u) <> v2 <> w <> (complex (diagRect 0 (1/sigma) (fst$size w) (fst$size u)))::Matrix C
  let (labmda,phi) = eig s :: (Vector C,Matrix C)
  return (labmda, u <> phi)

initial_energy::Vector C -> Matrix C -> Vector C
initial_energy v phi = (pinv (tr phi)) #> v



--{-
check :: Matrix C
check = fromColumns x
  where
    lam = diag (fromList [0.8,0.9,0.95])
    v_1 = fromList [1,0,0]::Vector C
    v_2 = fromList [0,1,0]::Vector C
    v_3 = fromList [0,0,1]::Vector C
    y = fromColumns [v_1,v_2,v_3] : map ( <> lam) y
    x = map (#> (fromList[1,1,1])) (take 10 y)

main = do
  (a,b) <- dmd (check)
  saveMatrix "real.txt" "%lf" (fst$fromComplex check)
  saveMatrix "complex.txt" "%lf" (snd$fromComplex check)
  print (initial_energy  (check #> fromList ([1]++replicate (snd (size check)-1) 0)) b)
  print b
--}
