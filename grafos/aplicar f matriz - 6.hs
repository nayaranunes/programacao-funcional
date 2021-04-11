import Data.Array

main::IO()
main = print("")


f x = x * x

somaMat f mat = array (bounds mat)
	[((i,j), f (mat!(i,j))) | (i,j) <- range (bounds mat)]