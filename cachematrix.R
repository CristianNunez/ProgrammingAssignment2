## makeCacheMatrix is a function that saves a matrix and can cache it's inverse
## cacheSolve gives de set value of makeCacheMatrix if it's NULL then calculates it and saves


## MakCacheMatrix makes a list that have the next elements
## set -> adjust the value of the list
## get -> prints the value of the set
## setinverse ->save the inverse value of the matrix
## getinverse -> prints the value of the saved inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse=matrix()) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function return the value of the inverse of a matrix.
## if the value is saved then it's shows this value
## if not then calculates the inverse trough the solve function

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
