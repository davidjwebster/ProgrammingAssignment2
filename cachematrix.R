## These functions calculate the inverse of a matrix. The matrix inverse is 
##stored in the cache, so that it can be looked up if required again.

## The first function, makeCacheMatrix, takes a matrix x and outputs a list of 
## four functions which can be used to look up or calculate the inverse 
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) xinv <<- solve
  getsolve <- function() xinv
  ##The output is a list of four functions as defined above which can be used to 
  ##store or look up x and xinv.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function finds the inverse of a matrix either by looking up an already 
## calculated value, or by direct calculation if not previously calculated - 
## in this case it stores the result so it can be looked up later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##Try finding a previously calculated value of xinv.
  xinv <- x$getsolve()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)  
  }
  
  ##If no previously calculated value of xinv exists, calculate it and store it for 
  ## later use.
  
  data <- x$get()
  xinv <- solve(data, ...)
  x$setsolve(xinv)
  xinv
}
