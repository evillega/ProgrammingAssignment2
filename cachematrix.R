## The following fuctions are used to cache the inverse of a mtrix


## makeCacheMatrix creates a matrix object that can cache its inverse
## Steps:
## Initialize the inverse property
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the value of the inverse of the matri
makeCacheMatrix <- function(x = matrix()) {
	## inv will store the cached inverse matrix
	inv <- NULL
	
	## sets the value for the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## Function to get the matrix
	get <- function() x
	
	##  Function to set the inverse of the matrix
	setinverse <- function(inverse) inv <<- inverse
	
	##  Function to get the inverse of the matrix
	get_inverse <- function() inv
	
	## Returns a list of the matrix
	list (set = set, get = get, 
		  setinverse = setinverse, 
		  getinverse = getinverse)

}

## The following function "cachSolve" calcultes the inverse of the "makeCacheMatrix" matrix above.
## It checks if the inverse has already been calculated.  If it is, then it skips the calculation.
## If not, then it calculates the inverse, sets the value in the cache using the set_inverse
##   function.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	
	## If the inverse is already calculated, then it will return it
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## If the inverse is not yet calculated, then this function calculates it
	data <-x$get()
	inv <- solve(data, ...)
	
	##  This function caches the inverse
	x$setinverse(inv)
	
	## return
	inv
      
}
