# 'makeCacheMatrix' creates a list 
# containing functions to set the value of 
# a matrix, get this value set its invere 
# and get it

makeCacheMatrix <- function(x = matrix()) {  
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
        setinv = setinv, getinv = getinv)
}

# 'cacheSolve' computes the inverse of "matrix" 
# returned by "makeCacheMatrix". If the 
# inverse has already been calculated, this function 
# retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
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
