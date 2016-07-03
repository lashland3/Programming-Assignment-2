# Programming-Assignment-2
## SetInverse and Get Inverse
## Create a special matrix by using the makeCacheMatrix Function
## Cache Matrix and cache the inverse
## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## CacheSolve will show the inverse of the matrix created above
## If the inverse has already been calculated then the
## cachesolve will retrieve the inverse from the cache.
## As seen in my test file

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
