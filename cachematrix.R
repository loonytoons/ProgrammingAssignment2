## The following functions allow you to create a cache store
## of a matrix and its inverse value, which you can use to
## repetitively extract the matrix's inverse value without
## running the expensive calculation each time
##
## Example usage:
## # create a cache object with your matrix
## matrixCache <- makeCacheMatrix$set(myMatrix)
## # get the inverse value
## cacheSolve(matrixCache)
## # change the matrix in the cache object
## matrixCache$set(newMatrix)


## This function creates a special "matrix" object that can
## cache its inverse
makeCacheMatrix <- function(cachedMatrix = matrix()) {
    # set the default inverse value
    matrixInverse <- NULL

    # set the matrix and reset the inverse value
    set <- function(matrix) {
        # only necessary if the matrix has changed
        if (!identical(matrix, cachedMatrix)) {
            cachedMatrix <<- matrix
            matrixInverse <<- NULL
        }
    }

    # return the set matrix
    get <- function() cachedMatrix

    # set the inverse value
    setInverse <- function(inverse) matrixInverse <<- inverse

    # return the cached inverse value
    getInverse <- function() matrixInverse

    # set the functions as a list in makeCacheMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve retrieves the inverse from the cache
cacheSolve <- function(matrixCache = makeCacheMatrix, ...) {
    # check if the cache has an existing inverse value
    matrixInverse <- matrixCache$getInverse()

    # return the cached inverse value if there is one
    if(!is.null(matrixInverse)) {
        return(matrixInverse)
    }

    # get the cached matrix
    cachedMatrix <- matrixCache$get()
    if(is.null(cachedMatrix)) {
        error("no cached matrix found")
    }

    # calculate the inverse value
    matrixInverse <- solve(cachedMatrix, ...)

    # cache the inverse value
    matrixCache$setInverse(matrixInverse)

    # return the inverse value
    matrixInverse
}