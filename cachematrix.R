## This pair of functions cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse. If the matrix changes (via $set), then the cached version 
## of inverse data is set to NULL, so when you get the inverse data with 
## cacheSolver function, it will recahce the inverse data based on the changed 
## matrix data.

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  
  set <- function(y) {
    x <<- y
    inversed <<- NULL
    message("Matrix has changed, cache has been cleared")
  }
  
  get <- function() x
  setInverse <- function(i) inversed <<- i
  getInverse <- function() inversed
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  inverse <- apply(data, FUN = solve, MARGIN = c(1,2))
  
  x$setInverse(inverse)
  inverse
}
