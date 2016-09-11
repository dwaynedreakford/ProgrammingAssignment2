## 'makeCacheMatrix' is a constructor function that packages a matrix
## with functions to manipulate the matrix. For the input matrix 'x', 
## the constructor returns a list, which contains the following named
## elements:
## 
## * 'set' - function that sets the matrix
## * 'get' - function that returns the matrix
## * 'setInverse' - function that accepts and stores the inverse of the matrix
## * 'getInverse' - function that returns the inverse of the matrix

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


## 'cacheSolve' calculates the inverse of the matrix 'x'.
## The inverse is cached when it is first calculated. So, if the
## inverse has already been calculated, the cached inverse is
## returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
