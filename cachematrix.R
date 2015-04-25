# These functions create a special matrix-like object that allows for caching of
# that matrix's inverse, so that it need be computed only once.

# makeCacheMatrix creates the matrix-like object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(solved) inv <<- solved
  list(set = set, get = get,
       getinv = getinv,
       setinv = setinv)
}


# cacheSolve is used to cache the inverse of the matrix-object returned by
# makeCacheMatrix, and is designed to work only with objects of that type

cacheSolve <- function(x, ...) {
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
