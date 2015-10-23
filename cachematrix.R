## makeCacheMatrix and cacheSolve functions below create a special object that
## stores a matrix and caches its inverse

## makeCacheMatrix is a function that takes a matrix x as an argument, and
## creates a special "matrix" object that can cache the inverse of x

## The following functions are created by makeCacheMatrix:
## set - this function is not called in makeCacheMatrix or cacheSolve, but can be
## called independently to reset the cached values of x and inv for a new matrix y.
## get - this function retrieves x
## setinv - this function caches the inverse of x as inv
## getinv - this function retrieves inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(arg) inv <<- arg
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix, puts it in the cache, and prints it. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache and prints it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}