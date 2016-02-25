## Priyanka Tirunagiri Script Date: 02/25/2016

##The purpose of this R function is to cache potentially time-consuming computations like calculating the inverse 
## of a matrix. This function calculates the inverse of a matrix and caches it for repeated usage instead of 
## calculating it multiple times.



## makecacheMatrix is a function which creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix() ){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {  ## Returns a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
