## This file has 2 functions.
## 1. makeCacheMatrix : This function creates a special matrix that could cache
##    the inverse of the matrix
## 2. cacheSolve : This function returns the inverse of the matrix

## makeCacheMatrix creates a special matrix. this special matrix could cache
## the inverse so that the inverse is retrieved from cache if the inverse is
## already calculated and is cached.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns the inverse for the given special matrix.
## cacheSolve will retrieve the inverse from the cache if it is already
## calculated. If the inverse is not cached then it will calculate the inverse
## and will cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
