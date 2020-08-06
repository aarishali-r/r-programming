## makes a matrix and finds its inverse

## makeCacheMatrix is a func that sets/gets the value of matrix and its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the special matrix mentioned above

cacheSolve <- function(x, ...) {
  cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
