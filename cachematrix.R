
## Theses functions work together to calculate the inverse of an ivertable matrix and cache the result in memory
## The makeCacheMatrix function creates a list will be  the argument for the cacheSolve function
## It assume that the matrix supplied is always invertible, error will happen for not invertable matrixes


## This function creates a special "matrix" object (list) that can cache its inverse using the casheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
        

