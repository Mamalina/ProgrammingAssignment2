## Matrix inversion is a costly computation.We are leveraging the concept of "caching"
## to reduce the cost of computation in the case where If "inverse of a matrix" has been already calculated,
## we are going to retrive the value(inverse matrix) from cache with out computing again.

## This function creates a special "matrix" object that can cache its inverse. This functions returns a 
## list which allows us to access its set,get,setInverse,getInverse method.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL  ## till the inverse of a matrix is not calculated it is NULL.
  set <- function(y){
    x <<- y
    i <<- NULL
  }  ## set method sets the value of a new matrix and makes the inverse to NULL.
  ##If we don't make "i" NULL, a previous value(inverse of another matrix) might be saved.
  get <- function()x  ## get method returns matrix x
  setInverse <- function(inv) i <<- inv  ## Sets the newly evaluted inverse of a matrix
  getInverse <- function() i ## returns the value of i which is inverse of a matrix.
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## a list to access its get,set,getInversea and setInverse function.

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  i <- x$getInverse() ##fetches the Inverse of matrix
  if (!is.null(i)){
    message("getting cache data")
    return(i)
  } ## if it has already been calculated simply returns the value that is present in cache
  data <- x$get() ## if not fetches the new matrix
  i <- solve(data) ## calculates inverse
  x$setInverse(i) ## and puts in the cache
  i 
  ## Return a matrix that is the inverse of 'x'
}
