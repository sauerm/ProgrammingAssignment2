## Two functions makeCacheMatrix and cacheSolve to store the cached result
## of the matrix operations. 

## Example Usage:
##    matrix =rbind(c(1, -1/4), c(-1/4, 1));
##    cache <- makeCacheMatrix()
##    cache$set(matrix)
##    cacheSolve(cache)


## makeCacheMatrix creates the cached objects to hide the cache functionallity
## from the users. The get/set sub-functions return/store the internal
## matrix objects. get/setInv returns/stores the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) cachedInv <<- inv
  getInv <- function() cachedInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve retruns the inverse of x by cheching for cached data and if nothing is cached it 
## caches the result and returns the invers by calling solve(x)
cacheSolve <- function(x, ...) {
  invM <- x$getInv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }

  invM <- solve(x$get())
  x$setInv(invM)
  return(invM)
}
