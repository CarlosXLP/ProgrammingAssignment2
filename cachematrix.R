## Helper function to calculate inverse of matrix. If the inverses has already
## been calculated, we can get the result value from a 'cache' place instead
## doing recalculation...

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## create a place for setting/getting inverse matrix
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Check for previous calculation of Inverse matrix. If found return it
    ## else calculate inverse matrix for returning
    
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

