## The functions makeCacheMatrix and cacheSolve provide a way to 
## compute and cache the inverse of a square invertible matrix.
## This means that the inverse for a matrix is computed once and cached, 
## all subsequent queries for the inverse of that matrix are completed by
## the cache.

## makeCacheMatrix creates an object comprising of a matrix object, 
## along with functions for storing and retrieving the matrix object and its inverse.
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(mat1) {
        mat <<- mat1
        inv <<- NULL
    }
    get <- function() mat
    setInv <- function(inv1) inv <<- inv1
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes as argument an object returned by makeCacheMatrix.
## makeCacheMatrix takes as argument a square invertible matrix.
## cacheSolve returns the inverse of the matrix, and caches it. All 
## subsequent queries are completed by the cached inverse object.
cacheSolve <- function(matInfo, ...) {
    inv <- matInfo$getInv()
    if (!is.null(inv)) {
        message ("getting cached data")
        return (inv)
    }
    
    mat = matInfo$get()
    inv <- solve(mat, ...)
    matInfo$setInv(inv)
    
    inv
}
