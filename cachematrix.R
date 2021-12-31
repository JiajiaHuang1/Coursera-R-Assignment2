## Function: Caching the Inverse of a Matrix
## The function can be used to cache the inverse of a matrix, including two functions as: makeCacheMatrix() and cachSolve().

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It includes the set,get, setinverse and getinverse functions.
## (1) Get() returns the vector x stored in the main function.
## (2) Set() changes the vector stored in the main function.
## (3) setinverse() and getinverse() are similar to set and get and don't calculate the inverse, it store the output value in m.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
