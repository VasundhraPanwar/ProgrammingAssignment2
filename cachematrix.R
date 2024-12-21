# makeCacheMatrix: Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse to NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix changes
  }
  
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter for the inverse
  getInverse <- function() inv
  
  # Return a list of all the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve: Computes the inverse of the special matrix object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, calculate it
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}
