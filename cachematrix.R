## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse. 
# It provides methods to:
# 1. Set the value of the matrix.
# 2. Get the value of the matrix.
# 3. Set the value of the cached inverse of the matrix.
# 4. Get the value of the cached inverse of the matrix.
# The caching mechanism helps avoid redundant computations.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for the inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when the matrix is reset
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


# cacheSolve
# This function computes the inverse of the special "matrix" object created by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed, 
# it retrieves the inverse from the cache instead of recomputing it.
# This improves efficiency for repeated calculations.


cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if it exists
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from the special object
  data <- x$get()
  
  # Calculate the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
