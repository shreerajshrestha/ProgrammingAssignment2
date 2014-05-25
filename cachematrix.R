## cachematrix.R
## Shree Raj Shrestha
## 5/16/2014
##
## "makeCacheMatrix" function initializes the inverse and vallidates the input matrix.
## It has sub-functions to get and set the value of the
## input matrix x and the cached inverse i from another environment.
## 
## "cacheSolve" function either retrieves stored inverse  cache or calculates (and sets)
## the inverse cache, i. This function uses the sub-functions of makeCacheMatrix
## to set, access and retrieve the values of x and i in the makeCacheMatrix environment.


## "makeCacheMatrix" function initializes the matrix and the cache.
## The "set" sub-function sets the value of x to input if and only if
## the input is a square matrix.
## The "get" sub-function returns the value of x, the matrix being processed.
## The "setinverse" sub-function sets the value of i, the cache, to the 
## calculated inverse from the cacheSolve environment.
## The "getinverse" sub-function returns the value of the cache i. 

makeCacheMatrix <- function(x = matrix()) {
    
    ## initializing the cache as NULL, the inverse is stored in this cache
    i <- NULL
    
    ## The "set" sub-function stores the input matrix y to x
    ## if and only if, y is a square matrix
    ## It also validates the input matrix and clears the cache i
    ## if x has been modified.
    set <- function(y) {
        
        if( ncol(y) == nrow(y) && class(y) == "matrix") {
            
            x <<- y
            i <<- NULL ## if x has been modified, clear the cache
            
        } else {
            
            message("Error! Please input a square matrix.")
            
        }   
        
    }
    
    ## return the value of x, the matrix being processed
    get <- function() x
    
    ## set the value of the cache i using 'inverse' from cacheSolve environment
    setinverse <- function(inverse) i <<- inverse
    
    ## return the cached inverse i from this (makeCacheMatrix) environment
    getinverse <- function() i
    
    ## output the functions in the makeCacheMatrix function as lists
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## "cacheSolve" retrieves the matrix and inverse 
## from the makeCacheMatrix environment (which is our cache).
## If the cache is full, returns the cached value of inverse
## If the cache is empty, calculate the inverse for the 
## retrieved matrix and sets the cache to the calculated value
cacheSolve <- function(x, ...) {
    
    ## get the cached inverse of x
    i <- x$getinverse()
    
    ## if cached inverse is not NULL, return the cached value and exit function
    if(!is.null(i)) {
        
        message("getting cached data")
        
        ### lines 17-20 are not executed because function ends after return
        return(i)
        
    }
    
    ## the subsequent lines of code are executed if i is NULL 
    ## (i.e x has not been input or x is modified)
    
    ## get the matrix being processed
    data <- x$get()
    
    ## calculate the inverse
    i <- solve(data, ...)
    
    ## set the value of chache to the inverse just calculated
    x$setinverse(i)
    
    ##return the calculated inverse
    i
    
}
