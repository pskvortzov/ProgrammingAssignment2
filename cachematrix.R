## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if ( ! is.matrix(x)) {
    stop('The argument must be a matrix!')
  }

  if (dim(x)[1] != dim(x)[2]) {
    stop('The argument must be a square matrix!')
  }
  
  s = NULL
  
  get <- function() x
  
  set <- function(y) {
    if ( ! is.matrix(y)) {
      stop('The argument must be a matrix!')
    }
    
    if (dim(y)[1] != dim(y)[2]) {
      stop('The argument must be a square matrix!')
    }
    
    x <<- y
    s <<- NULL
  }
  
  getsolve <- function() s
  
  setsolve <- function(solve) s <<- solve
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  
  if( ! is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  s
}
