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