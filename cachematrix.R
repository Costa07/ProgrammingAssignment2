## This functions creates a special matrix object that can cache its inverse
## and computes the inverse of said matrix returned.
## If the inverse has already been calculated (and the matrix has not changed),
## then should retrieve the inverse from the cache.

## Creates a special matrix and perform a 'cache' function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special matrix.
## If the inverse has already been calculated, should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data!")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}