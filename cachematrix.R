## Put comments here that give an overall description of what your
## functions do

## Translated from the makeVector() function in the course material
# Return a list with elements to get/set the matrix, as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  mymat <- NULL
  set <- function(y) {
    x <<- y
    mymat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) mymat <<- inv
  getinverse <- function() mymat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Translated from the cachemean() function in the course material
# Pull the cached value (inverse); if not null, return; else compute

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message('getting cached data')
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
