## Put comments here that give an overall description of what your
## functions do
## calculate the inverse of a matrix that's stored in a list of functions, 
## which could be retrieved without being recalculated.

## Write a short comment describing this function
## construct a list of functions used to store and retrieve a matrix "x" 
## and its inverse:
## $get() for retrieving the orginal matrix
## $set(y) for caching the matrix "y" to be inverted
## $getinv() for retrieving the inverted matrix
## $setinv(inv) for caching the inverted matrix "inv" 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## calculate the matric inverse from the func list x, constructed from "makeCacheMatrix()".
## if x has not been modified since last inversion, a cached version of the inverse is returned
## instead of re-calculating the inverse. The actual routine used for inversion is "solve()" for 
## a generic square matrix without checking for singular condition.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse )) {
        message("getting cached data")
        return(inverse )
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
