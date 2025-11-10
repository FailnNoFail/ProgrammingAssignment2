## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
}

  get <- function()x #gets x
  setInverse <- function(inverse) m <<- inverse # sets inverse to m
  getInverse <- function() m # gets m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function

## This function computes the inverse of the matrix if it hasn't been
## computed before or retrieves the answer that has already been calculated
cacheSolve <- function(x, ...) {
  m <- x$getInverse() # gets inverse or NULL if it hasn't been computed already
  
  if(!is.null(m)){ # if nverse has been computed before, return inverse
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # compute inverse if it hasn't been already
  x$setInverse(m)
  m
}
