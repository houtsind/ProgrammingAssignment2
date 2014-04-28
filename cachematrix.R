## Put comments here that give an overall description of what your functions do

## The makeCacheMatrix function creates a special matrix object that can cache
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix <- NULL
        
        set <- function(matrix) {
                x <<- matrix
                cacheMatrix <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(inverse) {
                cacheMatrix <<- inverse
        }
        
        getInverse <- function() {
                cacheMatrix
        }
        
        list (
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve function should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cacheMatrix <- x$getInverse()
        
        if(!is.null(cacheMatrix)) {
                message("getting cached data")
                return(cacheMatrix)
        }
        
        matrix <- x$get()
        
        cacheMatrix <- solve(matrix,...)
        
        x$setInverse(cacheMatrix)
        
        cacheMatrix
}
