## Matrix inversion can be a costly operation.  To work more efficiently while 
## analyzing data, it would be nice to cache inverted matrices so we can
## access them again without having to do the computation again.  These
## functions provide that functionality.

## accepts a matrix as a parameter and returns a "CacheMatrix", which can
## store the original data and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function accepts a "CacheMatrix" created by the previous function and
## returns its inverse.  If the CacheMatrix already contains the inverse in its
## cache, the cached data will be returned.  Otherwise, the inverse will be
## computed, stored in the cache, and returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
