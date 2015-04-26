# ### Assignment: Caching the Inverse of a Matrix
# This second programming assignment will require you to write an R
# function that is able to cache potentially time-consuming computations.
# The two following functions cache the Inverser of a Matrix.
# Matrix inversion can be a costly computation and thus these functions help with
# caching the inverse of a matrix rather than computing it
# repeatedly. For the purposes of this assignment, the assumption
# has been made that the matrix supplied to the functions will always be invertible.
# The two functions used for this assingment are named 'makecacheMatrix'and çacheSolve'
# and they will be described in more detail further on.
#
## A. makecacheMatrix
# The first function, `makecacheMatrix` creates a special "matrix" object, which is able to
# cache its inverse. This object is really a list containing a function to:
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inversed matrix
# 4.  get the value of the inversed matrix
#
makeCacheMatrix <- function(x = matrix()) {
## This function creates a list of functions based on 'x'.
  m <- NULL
  ## This function sets the value of the matrix.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## This function gets the value of the matrix.
  get <- function() {
    x
  }  
  ## This function sets the value of the  inverse of the matrix using the 'solve' function of R.
  setinverse <- function(solve) {
    m <<- solve
  }
  ## This function gets the value of the  inverse of the matrix.
  getinverse <- function() {
    m  
  }
  ## This is a list containing the 4 functions created above.
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}
# 
## B.cacheSolve 
# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. However, it first checks 
# to see if the inverse has already been calculated.If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache, skipping the actual computation.
# Otherwise, it calculates the inverse of the table and sets its value in the cache
# via the `setinverse` function. 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'.
  m <- x$getinverse()
  ## Checks if the inverse of 'x' has already been calculated before.  
  if(!is.null(m)) {
    ## The inverse already exists in the cache so the previously calculated matrix is returned.
    message("getting cached data")    
    return(m)
  }else{
    ## The inverse is calculated, stored in the cache and returned.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
  }
    
}
