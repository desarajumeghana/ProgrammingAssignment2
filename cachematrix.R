## R Programming Assignment 2 : Caching the inverse of a matrix
##e2e072488d02797d11684c667de6e37c10f39cb4

## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'inv' 
##    (which will be used to save inverse matrix latter, i.e. a cached data).
## 2. Provides function get() to obtain "raw" matrix of which one needs to find the inverse.
## 3. Provides function setInvmatrix() to assign computed inverse matrix (of x) to inv.
## 4. Provides function getInvmatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL
    getMatrix <- function() x
    setInvmatrix <- function(Invmatrix) inv <<- Invmatrix
    getInvmatrix <- function() inv

    # return a list of functions as an R object
    list(getMatrix=getMatrix, setInvmatrix=setInvmatrix, getInvmatrix=getInvmatrix)
}


## --------------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.

## --------------------------------------------------------------------------------
cacheSolve <- function(x) {
    inv <- x$getInvmatrix()
    if(!is.null(inv)){
        message("Cached data found. Getting result...")
        return(inv)
    }
    else {
        message("No cached data found. Calculating inverse of matrix...")
        data <- x$get() # obtains matrix from object x
        inv <- solve(data) # finds inverse matrix
        x$setInvmatrix(inv) # assigns resulting inverse matrix to object x
        message("Done.")
        return(inv)
    }
}
