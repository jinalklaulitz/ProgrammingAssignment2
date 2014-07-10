## The function written here checks if an existing computation of
## an inverse of a certain matrix exists and returns the value if it does
## or solves for if it doesn't exist already.

## makeCacheMatrix will create a list object that will set and maintain
## the value of a matrix and inverse of the matrix if it has been solve before

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will check if an existing inverse of a matrix exists
## and will return the computation or solve for it if it doesn't exist.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("returning inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
