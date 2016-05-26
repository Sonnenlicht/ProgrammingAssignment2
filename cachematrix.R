## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Special matrix function that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  nr <- nrow(x)
  nc <- ncol(x)
  inv <- matrix(data = NA, nrow = nr, ncol = nc)
  
  set <- function(y) {
    x <<- y
    inv <- matrix(data = NA, nrow = nr, ncol = nc)
  }
  
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        if (is.na(inv[1][1]) == FALSE) {
          message("getting cached inverse of matrix")
          return(inv)
        }
        
        A <- x$get()
        inv <- solve(A)
        x$setinv(inv)
        inv
        
}
