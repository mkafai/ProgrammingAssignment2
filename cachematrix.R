## The following two functions create a special matrix object and cache its inverse for future use. 
## 

## the makeCacheMatrix function creates a list of functions to set the values for a matrix, 
## get the values of a matrix, set the inverse of a matrix, and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## The cacheSolve function checks of the inverse of the special matrix from the above function has been 
## computed or not. If yes, it skips the computation and returns the cached value. If not, it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
