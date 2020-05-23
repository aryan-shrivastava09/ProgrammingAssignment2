## the makeCacheMatrix crates a special matrix object that is capable of creating its inverse.
## the cachesolve function returs a cached value of inverse (im) if there is any, and calculates and retuurns an inverse if a cached value is found to be NULL

## This function creates a special 'matrix' onject that can create its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get<- function() x
  setinverse <- function(solve) im<<- solve
  getinverse <- function() im
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im))
  { print("getting cached data") 
    return(im)}
  data<- x$get() ## Return a matrix that is the inverse of 'x'
  im<- solve(data,...)
  x$setinverse(im)
  im
}

