## The below two functions are used to create a special object that 
## stores a matrix and caches its inverse.

## The function makeCacheMatrix creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        MI <- NULL
        set <- function(y) {
                x <<- y
                MI <<- NULL
        }
        get <- function() x
        setmatrixInverse <- function(matrixInverse) MI <<- matrixInverse
        getmatrixInverse <- function() MI
        list( set = set, get = get, 
              setmatrixInverse = setmatrixInverse, 
              getmatrixInverse = getmatrixInverse)
}


## The cacheSolve function calculates the inverse of the special matrix returned by makeCacheMatrix above. 
## If the matrix has not changed, the function first checks to see if its inverse has already been calculated 
## and then retrieves the result from the cache and skips the computation. Otherwise, the inverse of the matrix 
## is calculated and the result stored in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MI <- x$getmatrixInverse()
        if(!is.null(MI)) {
                message("Getting cached data")
                return(MI)
        }
        matrix <- x$get()
        MI <- solve(matrix, ...)
        x$setmatrixInverse(MI)
        MI
}
