
## makeCacheMatrix creates enhanced matrix "object"
## matrix x, inverse x_inv
## getters/setters: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
      x <<- y
      x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Compute inverse for cache matrix "object" 
cacheSolve <- function(x, ...) {
  ## check if inverse already stored 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  ## compute and store inverse
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
