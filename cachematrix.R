## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix<-function(x = matrix()) {
  inv <- NULL
  ## set the matrix
  set<-function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the matrix
  get<-function() x
  ## function cache the inverse of the matrix
  setInv<-function(inverse) inv <<- inverse
  ## retrieve the inverse
  getInv<-function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x,...) {
  inv <- x$getInv()
  ## if the inverse is cached, return it
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  ## else, get the matrix
  data<-x$get()
  ## and calculate the inverse
  inv<-solve(data)
  ## cache the inverse
  x$setInv(inv)
  inv
}

## Test:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## The cache initally does not exist, calcuate inverse
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Fetch the cached inverse for subsequent runs
## > cacheSolve(m)
## Getting cached inverse.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## >
