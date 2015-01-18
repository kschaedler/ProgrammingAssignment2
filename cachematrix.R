
## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## The inverse initialised to NULL
  inv <- NULL
  ## sets the original matrix and re-initialises inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## returns current original matrix
  get <- function() x
  ## sets inverse
  setinv <- function(solve) inv <<- solve
  ## returns cached inverse
  getinv <- function() inv
  ## result: list of functions for getting / setting original matrix and inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Try to get chached inverse
  inv <- x$getinv()
  ## if inverse already in cache return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise get original matrix and compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  ## cache inverse
  x$setinv(inv)
  ## return inverse
  inv}
