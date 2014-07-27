## The following functions will cache the inverse of a matrix to make it
## computationally less intensive

## makeCacheMatrix will create a matrix object that can cache its inverse
## (from assignment problem statement)

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
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return matrix inverse from cache if available
## else will calculate it (from assignment problem statement)

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
