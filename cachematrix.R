## Coursera R Programming Class: rprog-009
## Programming Assignment #2
## Objective: Write two functions. One that will get/set data for a matrix and its inversion 
## and another that will test if the inversion of a matrix is cached/stored and if not, cache it.

## makeCacheMatrix - set/get that data for a matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheInverse - see if the inverse of matrix "x" is cached/stored. If not, invert and store it using the solve() function in R
cacheInverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
