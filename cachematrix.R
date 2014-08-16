# These functions "makeCacheMatrix" and "cacheSolve" are used in conjunction to be
# able to cache the results of calculating the inverse of a matrix as this can
# be a time consuming operation

# This function makes a custom matrix that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      # Set initial cached inverse value to NULL
      inverse <- NULL
      
      # set function sets the matrix content and clears the cached inverse
      set <- function(newMatrix) {
            x <<- newMatrix
            inverse <<- NULL
      }
      
      # get function returns the matrix content
      get <- function() x
      
      # setInverse function saves the calculated inverse to the cache
      setInverse <- function(newInverse) inverse <<- newInverse
      
      # getInverse returns the inverse from the cache
      getInverse <- function() inverse
      
      # Return the custom matrix
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

# This function returns the inverse of a matrix created by the above makeCacheMatrix
# function. If the matrix is unchanged and the inverse has already been calcuated by
# this function, it will return the inverse from the cache. This function assumes
# the the matrix is invertible (square).

cacheSolve <- function(x, ...) {
      # Check if the inverse has been calculated already
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("Returning cached data")
            return(inverse)
      }
      # Cache is null, calculate the inverse
      data <- x$get()
      inverse <- solve(data, ...)
      
      # Set the cache for next time
      x$setInverse(inverse)
      
      # Return the inverse
      inverse
}