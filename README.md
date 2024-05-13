# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix as NULL
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    # Reset the cached inverse when the matrix is updated
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to get the cached inverse
  getInverse <- function() inverse
  
  # Function to compute the inverse of the matrix
  cacheInverse <- function() {
    # Check if the inverse is already cached
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    # If not cached, compute the inverse
    inverse <- solve(x)
    # Cache the inverse
    message("Caching inverse")
    inverse <<- inverse
    # Return the inverse
    return(inverse)
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, cacheInverse = cacheInverse)
}

# Function to compute the inverse of a special matrix object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Get the cached inverse if available
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Inverse retrieved from cache")
    return(inverse)
  }
  # If inverse not cached, compute it
  inverse <- x$cacheInverse()
  return(inverse)
}
