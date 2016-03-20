## Submission by Musab M. Jafri

## The method returns a cacheable matrix with the following properties:
## get - returns the matrix
## set - sets a new version of the matrix
## getinverse - gets the cached inverse of the matrix
## setinverse - sets a new inverse value for the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The method returns the inverse of the provided cacheable matrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}