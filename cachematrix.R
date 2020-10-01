## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix contains functions to retrieve the matrix, set the inverse in the cache, or get the already-cached inverse
## cacheSolve checks whether the inverse is already cached, and if it's not, it sets the inverse

## Write a short comment describing this function
## Retrieve the matrix, set the inverse in the cache, or retrieve an already-cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Check whether the inverse is already cached, and if it's not, it sets the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
