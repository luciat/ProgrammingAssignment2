## Matrix inversion is usually a costly computation so one certainly gains some benefit
## from caching the inverse of a matrix rather than computing it repeatedly. 

## makeCacheMatrix creates a "special matrix" object associated with a invertible matrix that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the "special matrix" created with makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, this function
## should retrieve the inverse from the cache.

cacheSolve <- function(sm, x) {
        ## Get current matrix
        cx <- sm$get()
        
        ## If the matrix has not changed, retrieve its inverse if already calculated
        if (identical(dim(x), dim(cx)) && all(x == cx)) {
                ix <- sm$getinverse()
                if (!is.null(ix)) {
                        message("getting cached inverse")
                        return(ix)
                }
        }
      
        ## Compute inverse
        message("computing new inverse")
        sm$set(x)
        ix <- solve(x)
        sm$setinverse(ix)
        return(ix) 
}
