## Functions to cache the inverse of given matrix
## to avoid costly recalculation.

## Create matrix object to be able to set and get
## a specified matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverted) m <<- inverted
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## Retreive matrix from matrix object if present,
## otherwise calculate the inverse and store in the
## matrix object (created by makeCacheMatrix)

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
