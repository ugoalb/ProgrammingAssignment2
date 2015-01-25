## This tiny library caches the inverse of matrix

## makeCacheMatrix creates a matrix-like object that can store and 
## retrieve the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) minverse <<- solve
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve use the object created by makeCacheMatrix, and use
## the stored value if it was previously calculated. If not, it'll
## solve the inverse of the matrix and store it for future uses.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data, ...)
        x$setinverse(minverse)
        minverse        
}
