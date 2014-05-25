## this function does NOT calculate the inverse of the matrix ("x") -
## it relies on the cacheSolve function calling it properly
## 
## it initializes a holder for a cached inverted matrix ("inv")
## and creates a list of functions that allow another function to 
## get or set the value of "inv", or of the initial matrix "x"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function checks to see if there is a cached inverse
## for the matrix "x"
## if there is no cache (we get null back) we do the "solve" function
## on the matrix, and then assign that to the cache for next time.
## we also return the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}



