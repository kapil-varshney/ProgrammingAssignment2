## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of 
## a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {   ## set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x    ## get the value of the matrix
    setinv <- function(solve) inv <<- solve    ## set the inverse of the matrix
    getinv <- function() inv    ## get the inverse of the matrix
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) {    ## Checking if the inverse has already been calculated
        message("Getting cached data")
        return(inv)    ## get the inverse from cache, skip the computation
    }
    data <- x$get()
    inv <- solve(data, ...) ##    Calculating the inverse
    x$setinv(inv)
    inv    ## Return a matrix that is the inverse of 'x'
    
}
