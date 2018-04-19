## Caching the Inverse of a Matrix


## This function creates a list set functions
## to set and get the value of a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL ## starting value of inverse v = null
    set <- function(y) {  ## set value of matrix
        x <<- y
        v <<- NULL
    }
    get <- function() x  ## get value of matrix
    setinverse <- function(inv) v <<- inv ## set value of inverse, store in cache
    getinverse <- function() v            ## get value of inverse
    list(set = set, get = get,  ## list of functions
         setinv = setinverse,
         getinv = getinverse)
}


## This function calculates/retrieves from the cache the inverse of the matrix
## returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    v <- x$getinv()  ## assign value of the inverse using function getinv from previous list
    if(!is.null(v)) {
        message("getting cached data")
        return(v) ## get inverse from cache if it has already been calculated
    }
    data <- x$get() ## assigned value of matrix to 'data'
    v <- solve(data, ...) ## calculate inverse of matrix 'data'
    x$setinv(v) ## set value of inverse
    v ## return Inverse matrix
}
