## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly
## Below two functions can be used to cache the inverse of a matrix.

## Cached matrix with the following functions
## get()        returns the matrix
## set()        sets the matrix and erases the cached inverse matrix
## getinverse() returns the cached inverse
## setinverse() sets the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  getinverse <- function() inv
  setinverse <- function(inver) inv <<- inver
  list( get = get,
        set = set,
        getinverse = getinverse,
        setinverse = setinverse 
  )
}



## cacheSolve returns the inverse of the matrix
##
## It first checks if the inverse is already calculated, if yes
## returns it. If it's not yet calculated, it calculates it andI
## sets it via the setinverse() function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if ( !is.null( inv ) ) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Test code
#
# m = matrix(c(4, 2, 7, 6), 2, 2)
# mcache = makeCacheMatrix(m)
# cacheSolve(mcache)
# cacheSolve(mcache)


