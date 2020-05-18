##### create special form of cached matrix #####
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # 1. set the value of the vector
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # 2. get the value of the vector
    get <- function() x
    
    # 3. set the value of the inverse
    setinv <- function(inv) m <<- inv
    
    # 4. get the value of the inverse
    getinv <- function() m
    
    list(set=set, get=get, 
         setinv=setinv, 
         getinv=getinv)
}


##### calculate inverse of the matrix #####
cacheSolve <- function(x, ...) {
    m <- x$getinv()

    # check the inverse of the matrix & use it if already existed
    if(!is.null(m)){
        message("getting cached data")
      return(m)
    }
    
    # calculate inverse 
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)   # set the value
    m
}
