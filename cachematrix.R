##                    ****Caching the Inverse of a Matrix****
## Description:
## Matrix inversion is usually a costly computation and there may be some benefit
## to "caching the inverse of a matrix" rather than compute it repeatedly.

## In the present work, two functions are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {                               
                x <<- y
                inv <<- NULL
        }
        get <- function() x                                
        setInverse <- function(inverse) inv <<- inverse    
        getInverse <- function() inv                       
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve

cacheSolve <- function(x, ...) {
                                                           ## Return a matrix that is the
        inv <- x$getInverse()                              ## inverse of "x"
        if (!is.null(inv)) {
                message("getting cached data")             ## The operation is displayed
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
