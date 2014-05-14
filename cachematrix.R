## makeCacheMatrix creates the appropriate functions, cacheSolve solves and caches the inverse matrix
##Returns the vector of four functions as described bellow
makeCacheMatrix <- function(x = numeric()) {
    
    ## when we first make the matrix, the inverse is null
    inv <- NULL
    ## creates the function that sets the matrix x to y and inv to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## gets the matrix
    get <- function() x
    ## sets the the inverse matrix inv to a value z
    setinv <- function(z) inv <<- z
    ## gets the inverse matrix inv
    getinv <- function() inv
    ##returns the list of the four functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## solves the inverse matrix if not yet cached and returns it

cacheSolve <- function(x, ...) {
    ## gets the inverse matrix inv (will be null if not yet cached)
    inv <- x$getinv()
    ## if it is not null, then it returns the right inverse matrix
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## gets the appropriate matrix which has to be inverted
    data <- x$get()
    ## inverts the matrix
    inv <- solve(data, ...)
    ## sets the inverse to inve
    x$setinv(inv)
    ## returns the inverse matrix
    inv
}
