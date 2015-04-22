## Function Calculates Inverse of a Matrix and Cache it for future Use

## This Function Creates a Special Matrix, which is a list of functions to
## get the Cached Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set_matrix <- function(y){
		x <<- y
		m <<- NULL
	}
	get_matrix <- function() x
	set_inverse <- function(inverse) m <<- inverse
	get_inverse <- function() m
	list(set_matrix = set_matrix, get_matrix = get_matrix,
		set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function checks if the Inverse is in the cache. If it is , then returns
## the result otherwise calculates the inverse and stores it in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$get_inverse()
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}
	data <- x$get_matrix()
	I <- solve(data)
	x$set_inverse(I)
	I
}
