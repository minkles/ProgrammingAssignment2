## These two functions create a special kind of matrix whose
## inverse can be cached in order to save time on calculations.

## The function makeCacheMatix creates a list containing 4 functions 
## relating to a matrix object contained within makeCacheMatrix.
## The 4 functions are:
## makeCacheMatrix$set(y) <- sets the value of the matrix
## makeCacheMatrix$get() <- returns the value of the matrix
## setinverse(inverse) <- sets a value for the inverse of the matrix
## getinverse() <- returns the vale of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
        	x <<- y
        	i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.  If the inverse has already
## been calculated using cacheSolve, it simply returns the value in the cache.
## Otherwise, it stores the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
