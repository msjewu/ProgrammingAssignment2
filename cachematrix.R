# This code is meant to use two functions to calculate and cache the
# inverse of a matrix so that we don't need to compute it over and over
# again.

# The function makeCacheMatrix is a list containing a function to:
# 1. set the value of a matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The function cacheSolve calculates the inverse of the matrix created with
# the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the matrix and sets
# the value of the inverse in the cache via the setinv function.

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
