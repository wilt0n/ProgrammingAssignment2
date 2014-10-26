## Used to cache the inverse of a matrix or calculate the inverse of a matrix
## Because it is resource intensive

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse_1 <- NULL ##declares variable without assignment
	set <- function(y) { ##declares set function with arguement y
		x <<- y
		inverse_1 <<- NULL
	}
	
	get <- function() x ## gets the value of the matrix
	setInverse <- function(inverse) inverse_1 <<- inverse ## function set to inverse
	get Inverse <- function() inverse_1 ## retrives inverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) ## list of functions
}


## This function computes the inverse of the special "matrix". If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse_1 <- x$getinverse() ## retrieving variable
        if(!is.null(inverse_1)) { ## check to see if inverse exists
                message("getting cached data")
                return(inverse_1) ##if it does, then it'll return the inverse
        }
        data <- x$get() ## retrieving data
        inverse_1 <- inverse(data, ...) ## calculating inverse
        x$setinverse(inverse_1) ## setting inverse to a variable
        inverse_1 ## returns the inverse
}
