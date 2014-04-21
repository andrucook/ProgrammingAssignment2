## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # the inverse of the matrix
    inverse <- NULL
    
    # set the matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        x <<- NULL
    }
    
    # get the matrix
    get <- function() {
        x
    }
    
    # set the inverse
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    # get the inverse
    getInverse <- function() {
        inverse
    }
    
    # return special "matrix" object
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    # get cached value of matrix inverse
    inverse <- x$getInverse()
    
    # if it's not null, return it
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    # otherwise we need to calculate it
    inverse <- solve(x$get())
    
    # then cache it
    x$setInverse(inverse)
    
    # return the inverse
    inverse
}
