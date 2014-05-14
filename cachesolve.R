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