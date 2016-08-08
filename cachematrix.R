## Caching the Inverse of a Matrix
##  uses solve() to calculate the inverse
##  of a given matrix and by taking advantage of
##  R's scoping rules is able to cache it's results. 
##
## Complete Example: 
##    > mymat <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
##    > mcm <- makeCacheMatrix(mymat)
##    > cacheSolve(mcm)
##    .... outputs inverse ...
##    > cacheSolve(mcm)
##    .... outputs inverse from cache ...

## Creates a special "Matrix" which is really a list
##  contains a function to:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ### 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ### 2. get the value of the matrix
  get <- function() x
  
  ### 3. set the value of the inverse of the matrix

  setInverse <- function(inverse) inv <<- inverse
  
  ### 4. get the value of the inverse of the matrix
  
  
  getInverse <- function() inv
  
  # return our list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Given a makeCacheMatrix object, returns the inverse 
##   of the matrix
##
##  USAGE: cacheSolve(x, ...) - where 'x' is a
##         'makeCacheMatrix' object
##
##  (if it doesn't exist in cache it calculates it
##   and caches before returning)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) { # we got something...
    message("getting cached data")
    return(inv)
  }
  
  # cache isn't filled yet
  mat <- x$get()  # get it
  inv <- solve(mat, ...)  # solve it
  x$setInverse(inv) # set it
  inv # return it
}
