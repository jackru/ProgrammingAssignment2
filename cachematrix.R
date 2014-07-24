## The following functions enable retrieval of a matrix's inverse from
## a cache in the case that it has already been computed, in order to
## avoid unneccessary (and potentially expensive) duplication of work

## Returns a list object that allows read access to a matrix m,
## and read/write access to an object m.inv that is intended to
## store the inverse of m

makeCacheMatrix <- function(m = matrix()) {
    m.inv <- NULL
    getmatr <- function() m
    setinv <- function(inv) m.inv <<- inv
    getinv <- function() m.inv
    list(getmatr = getmatr, setinv = setinv, getinv = getinv)
}

## Computes and returns the inverse of the matrix embedded in the object
## created by makeCacheMatrix. If the inverse has already been calculated,
## cacheSolve should retrieve the inverse from the cache. Otherwise,
## it calculates the inverse, and stores it in the cache.

cacheSolve <- function(x) {
    x.inv <- x$getinv()
    if(!is.null(x.inv)) {
        message("getting cached inverse matrix")
        return(x.inv)
    }
    matr <- x$getmatr()
    x.inv <- solve(matr)
    x$setinv(x.inv)
    x.inv
}