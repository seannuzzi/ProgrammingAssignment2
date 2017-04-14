##
## This function will creat an object based upon a square matrix
## with functions to support matrix inversion and retrieving the
## original matrix
##
## Author: S. Eannuzzi
## Created: April 14, 2017
## 
## Usage: 
##    c <- matrix(c(1, 0, 3, 2, 2, 4, 3, 2, 1), ncol = 3)
##    cm <-makeCacheMatrix(c)
##    cm$getsolve()
##    cacheSolve(cm)

## Helper object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get Function to retrieve the original matrix
  get <- function()
    x
  
  # Local set solve function.  Grabs m from the parent (prior)
  setsolve <- function(solve)
    m <<- solve
  
  # Local get solve function
  getsolve <- function()
    m
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## This is the worker function that will either:
##  1. Solve (invert) the matrix or
##  2. return back a cache version of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Simple test for Matrix cache inversion
## Clear workspace
## rm(list=ls())
remove(c)
remove(cm)
c <- matrix(c(1, 0, 3, 2, 2, 4, 3, 2, 1), ncol = 3)
cm <-makeCacheMatrix(c)
cm$getsolve()
#NULL
cacheSolve(cm)
#[,1]       [,2]       [,3]
#[1,]  0.5 -0.8333333  0.1666667
#[2,] -0.5  0.6666667  0.1666667
#[3,]  0.5 -0.1666667 -0.1666667
cacheSolve(cm)
#getting cached data
#[,1]       [,2]       [,3]
#[1,]  0.5 -0.8333333  0.1666667
#[2,] -0.5  0.6666667  0.1666667
#[3,]  0.5 -0.1666667 -0.1666667

cm$getsolve()
#[,1]       [,2]       [,3]
#[1,]  0.5 -0.8333333  0.1666667
#[2,] -0.5  0.6666667  0.1666667
#[3,]  0.5 -0.1666667 -0.1666667

