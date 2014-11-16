## Coursera R Programming course - November 2014
## Programming Assignment 2
## Adapted by Mario Fiamenghi from the  "Caching the Mean of a Vector" example from Programming Assignment 2


## Theses functions work together to calculate the inverse of an invertable matrix and cache the result in memory
## The makeCacheMatrix function creates a list that will become the argument for the cacheSolve function
## It assume that the matrix supplied is always invertible, error will occur for not invertable matrixes


## This function creates a special "matrix" object (list) that can cache its inverse using the casheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## This function  uses the superassignment to set the matrix and its inverse outside this function's scope
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## this function just return the matrix
        get <- function() x
        
        ## this function uses the superassignment to set the inverse matrix outside the scope of function
        setinv <- function(inv) m <<- inv
        
        ## this function returns the inverse matrix
        getinv <- function() m
        
        ## this line creates a object (list) with function elements to be fed in the next function
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}




##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
        ## This block uses the getinv function to retrive the inverse matrix in memory if already exist
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If the inverse matrix have not been calculated before, then this block calculates and returns the inverse of the matrix
        data <- x$get()## retrives the matrix from the global evironment
        m <- solve(data, ...) ## assigns the inverse matrix
        x$setinv(m) ## salves the values of the inverse to the global environment
        m ## prints the inverse matrix to console
}
        

