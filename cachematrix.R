## The functions below operate on a matrix object which caches the matrix's inverse.
## makeCacheMatrix defines a matrix object, and contains methods for
## setting values for a matrix and its inverse, as well as retrieving these values.
## cacheSolve operates on the makeCacheMatrix object to calculate the matrix inverse
## and cache this value within the makeCacheMatrix object.

## makeCacheMatrix creates a "matrix" object that can cache the matrix's inverse.
##    inv is the matrix inverse.
##    The set function is used to define the matrix.
##    The get function is used to retrieve the matrix.
##    The setinv function is used to define the matrix inverse.
##    The get function is used to retrieve the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
