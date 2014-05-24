## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMatrix <- function(x = matrix()) {
    
    ## initializing the cache as NULL, the inverse is stored in this cache
    i <- NULL
    
    ## the "set" function stores the matrix passed by the user to x
    ## only square matrices are allowed as input
    set <- function(y) {
        
        ## if the matrix provided by the user is a square matrix,
        ## then store the matrix passed to the function to x,
        ## else show error message
        if( ncol(y) == nrow(y) && class(y) == "matrix") {
            
            x <<- y
            i <<- NULL
            
        } else {
            
            message("Error! Please input a square matrix.")
            
        }
        
        ## set the inverse matrix stored in cache to NULL
        ## this is necessary because x has been, or tried to be, modified
        ## and the cached inverse, i, is not the desired inverse
        
        
    }
    
    ## function that returns x, the matrix being processed for inverse
    get <- function() x
    
    ## function that stores the value of inverse passed into the cache
    setinverse <- function(inverse) i <<- inverse
    
    ## function that returns the current cached inverse
    getinverse <- function() i
    
    ## output the functions in the makeMatrix function as lists
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheinverse <- function(x, ...) {
    
    ## get the inverse of x
    i <- x$getinverse()
    
    ## if inverse is not NULL, return the cached value and exit function
    if(!is.null(i)) {
        
        message("getting cached data")
        
        ### lines 17-20 are not executed becuase function ends after return
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
