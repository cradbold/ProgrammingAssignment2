## This script contains complementary functions that create a 
## cache of a custom 'matrix' object.  Once created, the inverse
## of the 'matrix' object can be calculated-stored (via the 
## native 'solve' function) and retrieved.

## This function creates a custom 'matrix' object that caches 
## itself and its inverse (if set).
makeCacheMatrix <- function(m=matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## This function computes the inverse of the custom 'matrix' 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated and cached, then it retrieves the inverse 
## from the cache value.
cacheSolve <- function(m, ...) {
    inv <- m$getInv()
    if (!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    } else {
        x <- m$get()
        inv <- solve(x)
        m$setInv(inv)
        return(inv)
    }
}
