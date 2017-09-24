makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  collect <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  cache <- function() matrix
  
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(collect = collect, cache = cache,
       setInverse = setInverse,
       getInverse = getInverse)
}


 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  output <- x$get()
  inverse <- solve(output, ...)
  x$setInverse(inverse)
  inverse
}
