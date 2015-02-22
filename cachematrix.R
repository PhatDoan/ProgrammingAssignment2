## This module provides 2 functions to facilitate the caching of the inverse of a matrix.
## Once the inverse of a matrix is solved for the first time, its value is stored inside
## the environment of 'makeCacheMatrix' so it can be retrieved later as needed instead of
## having to solve again.



## Stores the calculated inverse (if any) and provide functions to get/set the matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initializes inverse matrix
  set <- function(y) { 
    # When the set function is called, set the matrix to given parameter y and also reset 
    # the stored inverse matrix value to NULL (i.e. there hasn't been a calculated inverse).
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Returns the stored matrix
  setinverse <- function(inverse) inv <<- inverse # Stores the given 'inverse' matrix as inv
  getinverse <- function() inv # Returns the stored inverse matrix 'inv'
  
  # Actual list containing functions that 'makeCacheMatrix' returns
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Calculates the inverse of a given matrix, but first checks if such inverse is stored in
## 'makeCacheMatrix'. If there is such calculated inverse, returns it instead of solving again.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Retrieves the stored inverse matrix in the 'cached' maxtrix
  if (!is.null(inv)){
    # If stored inverse is availabe then return it and quit. 
    message("getting cached inverse")
    return(inv)
  }
  # No inverse matrix has been stored, so retrieve the matrix and solve for its inverse
  mat <- x$get() 
  inv <- solve(mat)
  x$setinverse(inv) # Cache the calculated inverse for future use
  inv # return the inverse matrix
}


