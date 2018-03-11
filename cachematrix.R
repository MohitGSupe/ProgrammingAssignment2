## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Basically, this function is built to create a matrix of which inverse is to be cached.
# Main aspects of this function is to focus on:
# 1. Setting the matrix.
# 2. Getting the matrix.
# 3. Setting the inverse.
# 4. Getting the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse_2) inverse <<- inverse_2
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function
# This function's primary objective is to obtain the inverse of the matrix returned by the above function. i.e makeCacheMatrix.
# If there are no changes in the matrix after obtaining the inverse, the function will obtain the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$get_inverse()
	if(!is.null(inverse)) {
		message("Got the cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$set_inverse(inverse)
	inverse

}
