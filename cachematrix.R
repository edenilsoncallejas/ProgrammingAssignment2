## Create an object that stores a Matrix and its inverse

## makeCacheMatrix receives a X matrix parameter and returns a list of setters and getters
## functions

makeCacheMatrix <- function(x = matrix()) {
  MatrixInverse <- NULL
  set <- function(y) {
    x <<- y
    MatrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(pInverse) MatrixInverse <<- pInverse
  getInverse <- function() MatrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Given an Matrix returned by makeCacheMatrix function, calculate the matrix´s inverse
## and if the matrix hasn´t changed return a cached matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x
  MatrixInverse <- x$getInverse()
  if(!is.null(MatrixInverse)) {
    message("getting cached data")
    return(MatrixInverse)
  }
  NormalMatrix <- x$get()
  MatrixInverse <- solve(NormalMatrix)
  x$setInverse(MatrixInverse)
  MatrixInverse
}
