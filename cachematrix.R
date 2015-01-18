## Function makeCacheMatric creates a matrix object which can cache the inverse of a given invertible matrix. 
## Function cacheSolve returns a previosuly calculated inverse or computes the inverse if not.
##
## Usage: T <- matrix(c(100,200,300,400), ncol=2, nrow=2)
##        cacheMatrix <- makeCacheMatrix(T)
##        cacheSolve(cacheMatrix)


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


## Check for the existence of the inverse matrix object first
## and if it exists return the cached value. Otherwise,
## calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
    
  invMatrix <- x$getInvMatrix()
  
## If the object is not NULL then return the previously calculated inverse.

  if (!is.null(invMatrix)){
    message("Getting cached data")
    return(invMatrix)
  }

## If the object is NULL then calculate and return the inverse.

  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInvMatrix(invMatrix)
  invMatrix
}
