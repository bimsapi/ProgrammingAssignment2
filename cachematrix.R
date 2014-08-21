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

#simple method to test... 
testCacheSolve <- function(dim = 5) {
  #random 5x5 matrix
  mymat<-matrix(rnorm(dim*dim), ncol=dim)
  m<-makeCacheMatrix(mymat)

  #here's the inverse...
  a<-cacheSolve(m)

  #check for the caching...
  b<-m$getinverse()

  message(all.equal(a, b))

  #a * mymat should be the identity matrix...
  all.equal(mymat, mymat %*% (mymat %*% a))
}
