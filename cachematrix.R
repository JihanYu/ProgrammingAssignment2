## R functions to compute inversion of a matrix using cache ##

# makeCacheMatrix() : create a special "matrix" object
makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){		# Set the value of the matrix
		x <<- y
		m <<- NULL
	}
	get <- function() x		            # Get the value of the matrix
	setinv <- function(inv) m <<- inv   # Set the value of the inversion
	getinv <- function() m		        # Get the value of the inversion
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# cacheSolve() : computes the inverse of the special matrix 
cacheSolve <- function(x, ...){
	m <- x$getinv()			# matrix returned by makeCacheMatrix()
	# If the inverse of the matrix has already been calculaed,
	# this returns the inverse from the cache
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	# Otherwise, this calculates the inverse of the matrix
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)   # Set the value of the matrix in the cache
	m
}
