#### ProgrammingAssignment2
#### Author: Veronica Mandelli
#### date: 2020-04-14



## This script is composed by two functions. 
## The first function take as input a matrix (MUST be squared and non-singular) and create a "special matrix" that is able to both recall the 
## the numeric matrix and store and recall its inverse inside it.
## The second function calculate the inverse if the matrix, if the inverse is already store into it, it recall it without calculate it again.


## Title: makeMatrix description
## NB: input matrix MUST be a squared non-singulat matrix otherwise the function can not solve the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
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


## Title: cacheInverse description
## This function Return a matrix that is the inverse of 'x'
## if the matrix and its inverse are already stored in the cache it will recall it and reeport it, othervise it will calculate it ex novo end 
## then report it
## NB: input matrix MUST be a squared  non-singulat matrix otherwise the function can not solve the inverse of it.

cacheSolve <- function(x, ...) {
        
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



