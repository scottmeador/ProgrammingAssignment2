## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInvMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInvMatrix <<- NULL
  }
  
  get <- function() x
  
  getInvMatrix <- function() cachedInvMatrix
  setInvMatrix <- function(inverse) cachedInvMatrix <<- inverse
  
  list(set = set, get = get, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInvMatrix()
  if (!is.null(invMatrix)){
    message("Getting cached data")
    return(invMatrix)
  }
  
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInvMatrix(invMatrix)
  invMatrix
}
