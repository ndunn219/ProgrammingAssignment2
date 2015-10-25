## Two functions that work together to make it easy to get the
## inverse of a matrix - from cache if it has been cached or 
## by creating it (and then caching it) if has not yet been
## cached.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse the matrix, either from the cache or
## by computing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Uncomment below to test
## mymatrix = matrix(c(1,-1,1,1), nrow=2)
## mycachedmatrix = makeCacheMatrix(mymatrix)
## cacheSolve(mycachedmatrix)
