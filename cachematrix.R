#########################################################################################
## Written by:  Olga Hartoog
## Date:        26-01-2017
## Description:
## This set of functions enables the user to calculate the inverse of a matrix.
## Calculating the inverse, which is a costly operation, will only be done if necessary, 
## otherwise it will retrieve a previously stored value. The function accomplishes this 
## by caching the inverse as soon as it is calculated.
#########################################################################################

## The function makeCacheMatrix takes a matrix as input and returns a special object (a list) 
## containing the matrix, and has room to store, set and access its value as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve() returns the inverse of a matrix. It accepts only an output object 
## of makeCacheMatrix() as intput, and returns the inverse of the matrix that was fed to 
## makeCacheMatrix() and which is stored in this object. Once the inverse is calculated, the 
## inserted object is updated with this result. The cacheSolve() function will look for a cached 
## value first before doing the costly calculation.

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
  
}



