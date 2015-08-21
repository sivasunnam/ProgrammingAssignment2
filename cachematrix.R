## This script returns an inverse of a matrix from cache or by calculating it

## This function create getters and setters for a matrix and its inverse and stores it in a list 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Returns the inverse of matrix x from cache if it is available in cache else it will calculate

cacheSolve <- function(x, ... ) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
                if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
