## Functions below cache the inverse of a matrix.
## These functions are used to avoid recomputing
##the same inverse repeatedly.

## The makeCacheMatrix object below sets the matrix,
## gets the matrix, sets the inverse of the matrix,
## gets the inverse of the matrix and then combines
## this all into a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve object returns a matrix that is the 
##inverse of 'x' by first checking if this has been
##already been computed. If it has, cacheSolve returns
##the inverse. If it has not, this object gets the 
##matrix, finds the inverse, caches the results in inv
##and then returns the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
