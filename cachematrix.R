## Put comments here that give an overall description of what your
## functions do
##This function creates a special "matrix" object that can cache its inverse.

## Note if the embedded matrix is changed then the code nullifies the cached value of
## the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #m is the cached inverse
        ##intialise caches
        set <- function(y) {
                x <<- y 
                m <<- NULL #on initialisation, the cached inverse is nulled
        }
        ##return initialising matrix
        get <- function() x  
        ##set the inverse matrix
        setinverse <- function(solve) m <<- solve   
        ##return the inverse matrix
        getinverse <- function() m                  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated, 
# then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##check to see if inverse is cached
        m <- x$getinverse() 
        if (!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        #otherwise, use the embedded matrix to generate the inverse
        data <- x$get()
        m <- solve(data)
        ##cache the inverse
        x$setinverse(m)
        m
}
