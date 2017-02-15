## Week 3 Programming Assignment
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## This is to write an R function is able to cache potentially time-consuming computations
## We will take advantage of the scoping rules of the R language and how they can 
## be manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

m <- matrix(rnorm(4), 2, 2)
m
mm <- makeCacheMatrix(m)
cacheSolve(mm)
