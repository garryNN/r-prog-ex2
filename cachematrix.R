## this function create spec work environment (functions and cach) and output it
## fun setMatrix check is new input matrix the same as old matrix x and no deal, if the same

makeCacheMatrix <- function(x = matrix()) {
  x1 <- NULL
  getMatrix <- function() x
  setMatrix <- function(y) {
    if(!identical(x, y)) {
      x <<- y 
      x1 <<- NULL
    }
  }
  setInverse <- function(invX) x1 <<- invX
  getInverse <- function() x1
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  Mdata <- x$getMatrix()
  invM <- solve(Mdata)
  x$setInverse(invM)
  invM
}
