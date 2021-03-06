## function makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## function cacheSolve:      computes the inverse of the special "matrix" returned by 
##                           makeCacheMatrix. If the inverse has already been calculated 
##                           (and the matrix has not changed), then the cachesolve should 
##                           retrieve the inverse from the cache.

## Creates the special "matrix" object that can cache the value of its inverse matrix
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    ## Sets the value of the vector
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Gets the value of the vector
    get <- function() x
    ## Sets the value of the inverse matrix
    setinv <- function(inv) i <<- inv
    ## Gets the value of the inverse matrix
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
