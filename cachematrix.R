## The following functions below will help find the inverse of a matrix, if it is
## invertible, given a matrix object. 
## A matrix object can be created using makeCacheMatrix() function. The created
## matrix object has capability to store both the matrix input and matrix inverse result.
## In addition, if the matrix object already had the cached inverse result, the cacheSolve()
## function will just return it without spending expensive resource to re-compute it.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  ## Define a set() function to store matrix input data
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  ## Define a get() function to return matrix input data
  get <- function() x
  
  ## Define a getInverse() function to return the cached
  ## matrix inverse result, if available
  getInverse <- function() invMatrix
  
  ## Define a setInverse() function to store cached result
  ## of inverse matrix of X
  setInverse <- function(invX){
    invMatrix <<- invX
  }
  
  ## Define list of functions to be accessible
  list(get = get, set = set, 
       getInverse = getInverse, setInverse = setInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  
  if (!is.null(invMatrix)){
    message("Getting cached matrix inverse result")
    return(invMatrix)
  }
  
  ## Compute the matrix Inverse of X, assuming X is invertible
  invMatrix <- solve(x$get())
  
  ## Cache the inverse matrix of X for next retrieval
  x$setInverse(invMatrix)
  
  ## Display the inverse matrix on console output
  invMatrix
}
