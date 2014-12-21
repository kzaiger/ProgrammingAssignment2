## makeCacheMatrix creates a list from a passed matrix, stores the matrix in cache and creates functions to get and set the cached matrix.
## cachesolve returns a matrix that is the inverse of the passed matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Creates a list from a passed matrix
  ## Stores the matrix and what will be the cached value, which is initially set to NULL
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get functions to read cached matrix
  ## set functions to apply anonymous function solve()
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

