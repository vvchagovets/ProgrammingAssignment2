## Functions serve for creation a matrix object and for getting inverse
## for a stored matrix. The inverse is either calculated with solve()
## function (if the matrix is new or changed) or is taken from a cache

## Function creates an object that store a matrix and its invert in cache

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMtx) inv <<- invMtx
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)


}


## This function computes the inverse of the matrix object returned by makeCacheMatrix
## in the case of new or changed matrix or returnes cached inverse matrix 

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


