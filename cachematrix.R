############################################################################
#
# This script provides two functions:
# "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse.
# "cacheSolve": This function computes the inverse of the special "matrix" returned
#   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
#   then the cachesolve should retrieve the inverse from the cache.
#
# Record of revision:
#   Date        Programmer		Description of change
#	====		==========		=====================
#	17.06.2015  ChriguK         Original code
#   18.06.2015  ChriguK         Added comments
#
############################################################################


## The following function (functional) creates a special "matrix" object that
## can cache its inverse by using closures. It returns a list of four functions, namely:
##
## "set" sets the values of a given matrix & resets values of its inverse matrix
##  (setting inverse matrix to NULL) in the parent environment (i.e. the
##  environment in which the function was defined).
##
## "get" gets the values of the matrix, stored in the parent environment.
## 
## "setinv" sets the values of the inverse matrix in the parent environment.
##
## "getinv" gets the values of the inverse matrix, stored in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache by using the closures that come along with the special
## "matrix" object created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}