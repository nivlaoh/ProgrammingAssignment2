## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This matrix function stores the matrix in x,
## and if x has been re-written, it invalidates the inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(param) m <<- param
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets the inverse and checks that if it's
## available, it gets the cached data, else it inverses it and sets
## inverse to cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
