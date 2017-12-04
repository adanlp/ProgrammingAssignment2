## Caching the inverse of a matrix 
##
## In order to avoid the costly computation of the inverse of a
## matrix, specially when done iteratevely, these functions
## make use of scoping rules to cache the inverse of a matrix 
## previosly computed, returning directly the data without 
## recomputing but indicating it with a message. 
##
## Adanlp
## 03/DEC/2017



## This funtion creates the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}

## This funtion resolve if the inverse of the matrix has been previously
## computed, then retuning the value directly; otherwise, compute the
## inverse using the functions of the special "matrix"
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinv(i)
    i
}
