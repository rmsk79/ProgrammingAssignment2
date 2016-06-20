## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## This script contains a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.
## This object is actually a list containing functions that:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix's inverse
## get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 
  set <- function(y) {                            ## set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                             ## get the value of the matrix
  setinv <- function(inverse) inv <<- inverse     ## set its inverse
  getinv <- function() inv                        ## get its inverse
  list(set = set, get = get,                      ## This list is the special "matrix" object
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {                       ## checks for a cached inverse 
      message("getting cached inverse")
      return(inv)
    }
    data <- x$get()                           ## if no cached inverse, then the function
    inv <- solve(data, ...)                   ## calculcates one and prints it.
    x$setinv(inv)
    inv
}