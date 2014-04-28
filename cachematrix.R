## The Rscript contain 2 functions that supports faster retrieval of cached 
## matricies. The first function supports the setting and retrieval of the 
## original or cached matrix.

## Function makeCacheMatrix:
## The makeCacheMatrix function creates a special matrix object that can cache
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initializes the cached matrix and sets it as NULL
        cacheMatrix <- NULL
        
        ## set function allows user to set their own matrix, maintains the 
        ## cached matrix as NULL
        set <- function(matrix) {
                x <<- matrix
                cacheMatrix <<- NULL
        }
        
        ## get function retrieves the set matrix
        get <- function() {
                x
        }
        
        ## setInverse function enables user to set the inverse matrix, assumes 
        ## the inverse matrix is correct (i.e. no error checking)
        setInverse <- function(inverse) {
                cacheMatrix <<- inverse
        }
        
        ## getInverse function retrieves the whatever matrix is stored in the 
        ## cache. If nothing has been set in cache, NULL is returned from
        ## initialization (from above)
        getInverse <- function() {
                cacheMatrix
        }
        
        ##sets list of functions
        list (
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## Function cacheSolve:
## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve function should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieves the cachedMatrix from the makeCacheMatrix function
        cacheMatrix <- x$getInverse()
        
        ## Determines if there is a cached matrix stored, returns it if there is
        if(!is.null(cacheMatrix)) {
                message("getting cached data")
                return(cacheMatrix)
        }
        
        ## If there is no cached matrix stored, the inverse matrix is calculated
        matrix <- x$get()
        
        ## Inverse matrix caculated (using solve function)...
        cacheMatrix <- solve(matrix,...)
        
        ## and stored in cache
        x$setInverse(cacheMatrix)
        
        ## Prints the calculated inverse matrix
        cacheMatrix
}