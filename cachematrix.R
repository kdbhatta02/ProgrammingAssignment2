## Put comments here that give an overall description of what your
## functions do
# The following functions create a special matrix object that can cache its inverse.

## Write a short comment describing this function
# This function makes inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(Y) {
                x <<- Y
                M <<- NULL
        }
        get <- function() x
        setinvm <- function(invm) M <<- invm
        getinvm <- function() M
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
# The following function returns a matrix that is the inverse of 'X' 
        cacheSolve <- function(x, ...) {
                M <- x$getinvm()
                if(!is.null(M)) {
                        message("getting cached data")
                        return(M)
                }
                data <- x$get()
                M <- solve(data, ...)
                x$setinvm(M)
                M
}

