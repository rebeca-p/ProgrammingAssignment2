## These functions cache the inverse of a matrix

## The first function creates a "special" matrix object, which is actually a list containing functions that cache the
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
##cache via the setInverse function

cacheSolve <- function(x, ...) {
  
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
 