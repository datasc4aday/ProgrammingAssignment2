## 1. The following function creates a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x<<-y
  m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}

## 2. This function calculates the inverse of the matrix object 
## returned by makeCacheMatrix and returns a matrix that is 
## the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}