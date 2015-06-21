## Functions to work with a cached inverse of a matrix
## to save processing time.

## This function creates other functions to get and set
## a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) i <<- solve
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## This function attempts to get the cached data and return it.
## If there is no cached data it will be created by the function.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}
