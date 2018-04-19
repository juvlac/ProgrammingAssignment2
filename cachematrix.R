## Caching the Inverse of a Matrix


## This function creates a list of functions
## to set and get the value of a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) v <<- inv
    getinverse <- function() v
    list(set = set, get = get,
         setinv = setinverse,
         getinv = getinverse)
}


## This function calculates/retrieves from the cache the inverse of the matrix
## returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    v <- x$getinv()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinv(v)
    v
}
