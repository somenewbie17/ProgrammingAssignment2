## These functions work together to cache the inverse of a matrix.
## This avoids recomputing the inverse repeatedly for the same matrix,
## improving performance when dealing with costly matrix operations.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse to NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when the matrix is changed
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse
  getInverse <- function() inv
  
  # Return a list of all the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  data <- x$get()        # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)      # Cache the inverse
  inv                    # Return the inverse
}
