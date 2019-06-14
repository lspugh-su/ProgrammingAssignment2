## Author: lspugh-su
## Date: 6/13/19

## Two functions makeCacheMatrix & cachSolve

## makeCacheMatrix
## Description: creates matrix then caches the inverse of that matrix
## Arguments: matrix
## Returns: matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve
## Description: computes inverse of matrix returned by makeCacheMatrix
## Arguments: matrix (x) returned from makeCacheMatrix()
## Returns: inverse matrix (x)
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting chached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
