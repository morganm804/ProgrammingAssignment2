## These functions cache the value of the inverse of a matrix once computed
## so that the inverse does not need to be computed each time it is referenced

## makeCahceMatrix creates a list containing a function to 
## set/get the value of the matrix and set/get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix,
## checking first to see of the value is stored in the cache and using that value if it is

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
