## Coursera R Programming Class: rprog-009
## Programming Assignment #2
## Objective: write 2 functions. One to test if the inversion of a matrix is cached, and if not, cache it.
## The other function gets and sets the data for the matrix inversion.

## makeCacheMatrix - set/get a vector for caching

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve - see if matrix is stored, if not, invert it and store it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
