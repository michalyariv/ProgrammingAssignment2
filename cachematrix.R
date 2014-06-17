## cachematrix.R provides functions that will allow the user to invert a matrix and cache the results.
## Subsequent calls for the inverted matrix will be read from the cache.
## Example usage: cacheSolve(makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)))

## makeCacheMatrix takes a matrix and returns an object that provides
## the getters and setters for the original matrix 
## as well as functions to get and set the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get=get, setinv = setinv, getinv = getinv) 
}


## The cacheSolve function takes a makeCacheMatrix object and performs solve() to invert.
## No error handling provided to validate the matrix is invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
