## These two functions create a special object 
## that stores a matrix and caches its inverse

## makeCacheMatrix creates a "matrix" object which is a list of 4 functions to:
## set and get the matrix, set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv)  inv <<- new_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve gives the inverse of a "matrix" object created by makeCacheMatrix
## It first checks whether the inverse has already been calculated,
## if so, it gets it from the cache and returns it,
## else, it computes it, puts it in cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
