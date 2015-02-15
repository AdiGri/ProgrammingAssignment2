## The functions will receive a matrix as an argument, calculate its inverse and store it in a cache. 
## Considering that the computing the inverse of a large matrix is a resource intensive
## process, the result will be saved in a cache for future reference. 
## The function will verify if the inverse of the matrix is stored in the cache and 
## return the cache value if the inverse of the matrix has been computed before.

## Creates the special matrix and the functions associated with storing and retrieval of the cache.

makeCacheMatrix <- function(x) {
        if(!is.matrix(x)) stop("The argument must be a matrix!")
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Verifies if a matrix has its inverse stored in the cache. If the cache contains
## an entry, it will be returned, otherwise, the inverse is computed using the solve function.

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
