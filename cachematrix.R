## The two functions makeCacheMatrix () and cacheSolve() work together to
## calculate the the inverse of a square matrix 'x' and cache the result.

## The function makeCacheMatrix() takes a square matrix and creates a list
## of 4 closure functions: set(), get(), setinv() and getinv()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve() first looks in getinv() if the inverse of x
## has already been calculated. If yes, it returns that solution and a
## corresponding message.
## If there is no inverse matrix for x so far, it is calculated now and
## then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
