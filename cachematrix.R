## This file defines two functions to optimize the solve() for a matrix.  
## First function takes the matrix and creates a special vector from it 
## to cache the inverse of that matrix - by providing 4 functions - 
##     get/set/getInv/setInv
## Second function uses that special vector to return cached inverse 
## or calculate, cache and return inverse.

## The first function, makeCacheMatrix creates a special "vector", which 
## is really a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(mInv) inv <<- mInv
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The following function calculates the inverse of the special "vector" 
## created with the above function. Since solve function is expensive, 
## and we may need to call this multiple times,
##    First check to see if the inverse has already been calculated. 
##    If cached Inv value is not NULL, return cached inverse
##    Else calculate and cache the inverse of the data via setInv and return.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
	                 return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
