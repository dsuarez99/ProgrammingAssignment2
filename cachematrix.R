## This functions ae intended to help make faster the repetitive
# calculation of the inverse of a matrix, if the inverse for a given matrix
# was already calculated and saved in the cache, it is not calculated over again
# and return the previous value, if instead it is the forst time the inverse is
# calculated to the matrix then the inverse is calculated and the answer stored in the
# cache and returned to the user.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve  
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
