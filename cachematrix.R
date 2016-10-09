## Functions to compute the inverse of a matrix and cache the result to
## minimize the number of needed computations.

## makeCacheMatrix creates a matrix that tracks if its inverse has been
## computed or not. If changes are made to the matrix the cached inverse is
## set to null. Made for use with the function cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # First initialization of the matrix, inverse is Null
  
  # Create setter, nulls the "cacheExists" variable whenever value is changed
  set <- function (y) {
    x <<- y # Scoping assignment needed since x is outside of function
    i <<- NULL # Reset the inverse
  }
	get <- function() x # Returns the matrix itself
  setInv <- function(inv) i <<- inv # Computes the inverse
  getInv <- function() i # Returns the inverse
  list(set = set, get = get,
       setInv = setInv, getInv = getInv) # Return list
}


## Solves for the inverse of a matrix created by the function makeCacheMatrix.
## If the inverse already exists, the cached version is returned.
cacheSolve <- function(x, ...) {
	i <- x$getInv()
	if(!is.null(i)) { # If i is not null there is cached data.
		message("Inverse already cached, getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...) # Solve for inverse of x
	x$setInv(i) # Save new value
	return(i)
}
