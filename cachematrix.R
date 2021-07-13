##I input first the 'x' as the matrix
##And then I set the "na" as a null
##After this, I changed every reference "solvevalue" to "nextanswer"
##There are two functions used in this program: makeCacheMatrix, makeCacheMatrix
##The function makeCacheMatrix contains of set, get, setnextanswer, getnextanswer
makeCacheMatrix <- function(x = matrix()) {
  na <- NULL
  set <- function(y){
    x <<- y
    ##initializing solved answer as NULL
    na <<- NULL
  }
  ##this function is used to get the matrix 'x' 
  get <- function() {x}
  setnextanswer <- function(nextanswer) {na <<- nextanswer}
  getnextanswer <- function() {na}
  list(set = set, get = get, setnextanswer = setnextanswer, getnextanswer = getnextanswer)
}

##I did the same procedure in the first part by changing every reference "solvevalue" to "nextanswer"
##this function is going to process the cache data
cacheSolveValue <- function(x, ...) {
  na <- x$getnextanswer()
  ##this function will be able to determine whether the solved answer is null
  if(!is.null(na)) {
    ##a message saying "getting cached data" will appear if the solved answer is not null
    message("getting cached data")
    ##this function will return to the solved answer
    return(na)
  }
  mat <- x$get()
  ##this function is used to process and determine the solved answer
  na <- solve(mat, ...)
  x$setnextanswer(na)
  ##then finally, this will return to the matrix of the solved answer of the 'x'
  na
}

## ---------------To Check the Program------------------------
##m <- matrix(rnorm(16),4,4)
##m1 <- makeCacheMatrix(m)
##cacheSolve(m1)

##            [,1]        [,2]       [,3]        [,4]
##[1,] -0.39382558 -0.15730317  1.0575477 -1.35766157
##[2,]  0.09481498 -0.01105367 -0.8364245 -0.00101470
##[3,]  0.45039487 -0.51612006 -1.2292370  2.00333309
##[4,]  0.38911951 -0.89142815 -2.7511054  4.17150935
## 
##The program is successfully working!

