## These functions demonstrates the caching the inverse of a matrix
## through storing a matrix then inverting it, which reduces time.


## This function creates a "matrix" object that can be fed in to 
## the second function cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the "matrix" created by 
## the function above, makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()  ## This returns a matrix that is the inverse of the vector 'x'
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


## This is some code to test the above functions.

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()


