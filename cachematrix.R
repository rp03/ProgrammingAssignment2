## A pair of functions that cache the inverse of a matrix.


## makeCacheMatrix(x) creates a special "matrix" object (list) that can cache the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setmean = setinverse,
             getmean = getinverse)

}


## cacheSolve(x) computes the inverse of the special "matrix" returned by makeCacheMatrix() above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
