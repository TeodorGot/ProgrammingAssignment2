
## Function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL ## make inverse to null
        ##set the matrix:
        set <- function(Matrix_O) {
            x <<- Matrix_O  ##Stored matrix outside of current environment
            Inverse <<- NULL ##reset inverse to null when matrix changed
        }
        ##get the matrix:
        get <- function() {
             x
        }
        ##set Inverse of matrix:
        setInverse <- function(solve) {
            Inverse <<- solve ##Stored Inverse outside of current environment
        }
        ##get Inverse of matrix:
        getInverse <- function() {
            Inverse
        }
        ##return list:
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Fuction  computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        Inverse <- x$getInverse() ##Return a matrix that is the inverse of 'x'
        ## checks to see if matrix has already been calculated. 
        ##If so, gets the matrix from the cache and skips the computation
        if( !is.null(Inverse) ) {
            message("getting cached data")
            return(Inverse)
        }
        ##Otherwise get matrix is retrieved from the list
        data <- x$get()
        Inverse <- solve(data)
        x$setInverse(Inverse)
        Inverse
}
