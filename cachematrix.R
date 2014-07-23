## Create a matrix type that is able to cache its inverse

## Create an object which stores a matrix and methods to read/write it and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
	x <<- m
	inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If cached inverse is available, return it, otherwise compute it and cache

cacheSolve <- function(x, verbose=FALSE, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
	if (verbose) message("getting cached data")
	return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
