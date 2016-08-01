
## Assignment is to cache the inverse of a matrix
## Assignment requires two functions to be created
## 1st function 'makeCacheMatrix' creates a matrix that can cache its inverse
## 2nd function 'cacheSolve' computes the inverse of the matrix


## 1st function 'makeCacheMatrix'
## Like example using 'mean' function, uses 'solve' to calculate inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

## 2nd function 'cacheSolve'
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}

