## function makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## function cacheSolve:      computes the inverse of the special "matrix" returned by 
##                           makeCacheMatrix. If the inverse has already been calculated 
##                           (and the matrix has not changed), then the cachesolve should 
##                           retrieve the inverse from the cache.

## Creates the special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    ## function to store matrix and its cached inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
## If the inverse matris is cached return the cached matrix,
## if not calculate the inverse, cache it and return the inverse.
cacheSolve <- function(x, ...) {
    ## Gets the inverse matrix from cached data        
    i <- x$getinv()
    ## If the inverse matrix exists return the cached inverse matrix.
    if(!is.null(i)) {
        return(i)
    }
    ## Otherwise gets the matrix and calculate the inverse.
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
