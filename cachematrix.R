## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
