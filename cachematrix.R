#this is a function has functions to get the matrix,get the inverse
#,set the matrix,set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The following function calculates the inverse of the Matrix 
#created with the above function. This function first checks  if
#the inverse has already been calculated. If so, it gets the inverse from
#the cache and skips the computation. Otherwise, it calculates the 
#inverse matrix and sets the value of the inverse in the cache via the
#setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}