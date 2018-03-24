## functions that are able to cache potentially time-consuming computations.
## In this case it will be used to solve matrix inversion. The first function
## 'makecaCheMatrix' is used to create an object (eg, 'mymatrix') that includes 
## the calculation (matrix inversion) and defines setting and getting the cached
## matrix inverse 

## calculates matrix inversion using solve(A) and assigns value to new object

makeCacheMatrix <- function(x = matrix()) {
        # calculates matrix inversion using solve(A).
        #
        # Args:
        #   x: a passed singular matrix
        #   xinv: solution to solve(x)
        # 
        # Functions:
        #   set: stores 'x'
        #   set: retireves 'x'
        #   setxinv: calls solve(x) and assigns solution to xinv
        #   getxinx: retrieves xinv
        #   
        # Returns:
        #   assigns names 'set', 'get', 'setxinv' and 'getxinv' to functions of
        #   the same name 
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setxinv <- function(solve) xinv <<- solve(x)
        getxinv <- function() xinv
        list(set = set, get = get,
             setxinv = setxinv,
             getxinv = getxinv)
}


## checks to see if matrix inverse is cached and retrieves cached value
## or calcualtes solution and caches the value

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # 
        # Args:
        #   x: a passed object returned from 'makeCacheMatrix'
        #   xinv: matrix inverse
        #   data: a singlular matrix given to 'makeCacheMatrix'
        #   
        # Returns:
        #   matrix inverse
        xinv <- x$getxinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setxinv(xinv)
        xinv
}
