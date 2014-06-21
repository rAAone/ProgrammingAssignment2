

## This function takes in a vector as argument and transforms it into a square matrix
## make sure that the length of the input is a perfect square so as to get a square matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    dim(y)<-c(sqrt(length(y)),sqrt(length(y)))
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function returns the cached inverse or calculates and then returns the inverse of a square matrix if an inverse of the matrix exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
