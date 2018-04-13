import Numeric.LinearAlgebra

--dmd

main = do
  observable <- loadMatrix "test.txt"
  let (u,sigma,v) = svd observable :: (Matrix Double,Vector Double, Matrix Double)
  let org = u <> (diag sigma) <> (tr v)
  print org
