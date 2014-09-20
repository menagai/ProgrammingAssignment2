## This script was written for the programming assignment 2
## for the Coursera course "R Programming"and have two functions,
## makeCacheMatrix and cacheSolve

## The makeCacheMatrix function creates a special matrix (list) that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function( y ){
        x <<- y
        m <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(inverse){
        m <<- inverse
    }
    getinverse <- function(){
        m
    }
    list(set = set, get = get
       , setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function compute the inverse of the special matrix returned by the makeCacheMatrix function. If the inverse has already been calculated, then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if( !is.null(m) ){
        message('getting cached data' )
        return(m)
    }
    data <- x$get()
    m <- solve( data )
    x$setinverse(m)
    m
}
