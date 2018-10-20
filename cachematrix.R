## Caching the Inverse of a Matrix :
## This script contains two functions. See below for the functions list and comments 
## on their functionality.
##      1) makeCacheMatrix: This function creates a special "matrix" object that can
##         cache its inverse.
##      2) cacheSolve: This function computes the inverse of the special "matrix" 
##         returned by makeCacheMatrix above. If the inverse has already been calculated 
##         (and the matrix has not changed), then the cachesolve should retrieve the 
##         inverse from the cache.

## Below is the function makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
        imtx <- NULL
        set <- function(y) {
                x <<- y
                imtx <<- NULL
        }
        get <- function() x
        setInvmtx <- function(InverseMtx) imtx <<- InverseMtx
        getInvmtx <- function() imtx
        list(set = set, get = get,
             setInvmtx = setInvmtx,
             getInvmtx = getInvmtx)
}

## Below is the function cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imtx <- x$getInvmtx()
        if(!is.null(imtx)){
        message("fetching cached inverse mtx")
        return(imtx)
        }
        mtx <- x$get()
        imtx <- solve(mtx)
        x$setInvmtx(imtx)
        imtx
}
