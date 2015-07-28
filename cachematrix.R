## Project 2 of R programming 
## Author: Miao 
## Date: July 26, 2015

## Example of use / implementation of tasks: 
## > source ('cachematrix.R')
## > x <- makeCacheMatrix (matrix (c(2,0,0,2), c(2,2))          
## > cacheSolve (x)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5


## The function makeCacheMatrix creates a matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the inverse is already calculated, it will instead find it in the cache 
## and return a inverse matrix object (not calculate it again).

makeCacheMatrix <- function(x = numeric()) {
    ## inverse matrix
    invMatrix <- NULL
    
    ## set matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x
    
    ## set inverse
    invMatrixSet <- function(inverse) invMatrix <<- inverse
    ## get inverse
    invMatrixGet <- function() invMatrix
    
    # Return the matrix 
    list(set = set, 
         get = get, 
         invMatrixSet = invMatrixSet, 
         invMatrixGet = invMatrixGet
)
}

## The function cacheSolve calculates the inverse of the matrix 
## created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    invMatrix <- x$invMatrixGet()
    
    # If the inverse is already calculated:: return inverse
    if (!is.null(invMatrix)) {
        message("Getting Cached Inverse Matrix")
        return(invMatrix)
    }
    
    # The inverse isn't yet calculated:: calculate inverse
    data <- x$get()
    invMatrix <- solve(data, ...)
    
    # Cache the inverse
    x$invMatrixSet(invMatrix)
        
    ## Return a matrix that is the inverse of 'x'
    invMatrix
}
