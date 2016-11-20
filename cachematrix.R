## MakeCacheMatrix forms a list of four elements containing functions for
## setting the value for matrix, getting matrix, setting the value of inverse
## and getting the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv<- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## CacheSolve function firstly checks if the inverse of a matrix is 
## already available. variable i contains the value of inverse if it is available
## In case it isn't, function get() is used to get the data and solve to calculate 
## inverse. The value of setinv is set to the value of inverse i.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
