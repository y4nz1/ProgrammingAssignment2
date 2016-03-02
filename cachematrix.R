## Coursera - R Programming
## Week 3 Programming Assignment 2 - Lexical Scoping
## Objective : To return the inverse of a matrix and cache it.  If the matrix is
##             not change, to return the cached matrix value, instead of re-
##             calculating.


## makeCacheMatrix creates a special "vector", which is really a list containing
## functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list (set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Calculates the inverse matrix of the matrix created with the makeCacheMatrix
## function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of inv in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting chached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
