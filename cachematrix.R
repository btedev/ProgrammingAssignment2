## To speed performing repeated inversions of matrices, the functions
## below will cache the first inversion and return the contents
## of the cache for repeated calls.

## Creates a list that has functions to set a matrix, get the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of a "matrix" object created by makeCacheMatrix.
## If the inverse has already been calculated, it will be returned
## from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## My test of the functions:
#
# > source("cachematrix.R")
# > mcm <- makeCacheMatrix(x=matrix(c(4,7,2,6), nrow=2, ncol=2, byrow=T))
# > cacheSolve(mcm)
      #[,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > cacheSolve(mcm)
# getting cached data
      #[,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

