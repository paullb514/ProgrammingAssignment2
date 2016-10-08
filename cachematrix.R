## Matrix object
## Setter, getter, and set/get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the matrix contents
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get matrix contents
  get <- function() x
  
  ## set the inverse when calcuated
  setinv <- function(inverse) inv <<- inverse
  
  ## get the inverse when calcuated
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of the matrix, retrieving it from the 
## cache stored inside the matrix object, if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
