##################
## Assignment 2 ##
##################

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
	inversedMatrix <- NULL 

	set <- function(y) {
		x <<- y
		inversedMatrix <<- NULL
	}

	get <- function() x

	setInverse <- function(inversed) inversedMatrix <<- inversed

	getInverse <- function() inversedMatrix

	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	m <- x$getInverse()
	
	## Verifies that the value m, stored previously with getInverse() 
	## exists and is not NULL. If it exists in memory it simply returns 
	## a message and the value m.
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
    
	data <- x$get()

	## Use the solve function to set the inverse of the matrix to m 
	m <- solve(data, ...)

	x$setInverse(m)
	
	m
}