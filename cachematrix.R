## Together, the two functions below provide the opportunity, for an input
## non-singular matrix, to return an inverse matrix in an effective way, by
## caching the inverse matrix in case the function is called multiple times.

## makeCacheMatrix defines functions for cacheSolve to use and enables 
## the caching of the output. cacheSolve is the primary called function, 
## which calls makeCacheMatrix to retrieve the inverse matrix.


## The makeCacheMatrix function makes use of lexical scoping to cache the 
## inverse of a matrix.
## It provides calling functions with the opportunity to set the original 
## matrix, get the original matrix, set the inversed matrix and get the
## cached inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The cacheSolve function takes a matrix and uses makeCacheMatrix() to
## either get a cached matrix or set a new one before returning the 
## inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
