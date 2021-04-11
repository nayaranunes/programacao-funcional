import Data.Array

main::IO()
main = print(transposta mat)


mat::Array(Int,Int)Int
mat = array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),3),((2,2),4)]

transposta::Array(Int,Int)Int->Array(Int,Int)Int
transposta mat = array ((li,ui), (lj,uj))               
  [((i,j),mat!(j,i)) | i <- [li..lj], j <- [ui..uj]]
    where ((li,ui), (lj,uj)) = bounds mat
