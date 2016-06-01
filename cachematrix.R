## Matrix inversion is usually a costly computation. So this code create a "special matrix" that 
##can cache its inversion. 

##MakeCacheMatrix creates a special "Matrix" which is really a list containing a function to
##
##set the value of the matrix
##get the value of the matrix
##set the value of the solve
##get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}

##CacheSolve calculates the solve of the special "matrix". 
##it returns value from cache if the solve has already been calculated. 
cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
        
}
