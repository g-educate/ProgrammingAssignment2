# The two functions makeCacheMatrix and cacheSolve work in conjunction to store
# a matrix and it's inverse and return the inverse that's already computed if
# it's available or compute it, store (cache) it and return it if the inverse
# was never computed before.

# ------------------------------------------------------------------------------
# makeCacheMatrix returns a list of 4 elements all of which are functions and it
# also internally stores (caches) a matrix and it's inverse. The four functions
# are named setMatrix, getMatrix, setMatrixInverse and getMatrixInverse. This
# function directly takes a matrix as an optional argument and caches it.
# Alternatively, the setMatrix function can be used to set (cache) the matrix.
# ------------------------------------------------------------------------------

makeCacheMatrix <- function(matrixData = matrix()) {
  # if the input argument is not a matrix object
  # notify the user about the same
  # and exit the function
  if(!class(matrixData)=="matrix") {
    message("Input is not a matrix")
    return()
  }
  
  # initialize the inverse
  matrixInverse <- NULL
  
  # the setMatrix function takes a matrix object as an input argument and caches it
  # in addition it also initializes the inverse of the matrix
  setMatrix <- function(y) {
    matrixData <<- y
    matrixInverse <<- NULL
  }
  
  # the getMatrix function returns the internally stored (cached) matrix
  getMatrix <- function() matrixData
  
  # the setMatrixInverse function takes a matrix object as an input argument
  # and caches it as the inverse of the matrix
  setMatrixInverse <- function(matrixInverseData) matrixInverse <<- matrixInverseData
  
  # the getMatrixInverse function returns a matrix object
  # that is considered to be the cached value of the inverse of the matrix
  getMatrixInverse <- function() matrixInverse
  
  # create a list of the above defined four functions
  # and returns it as the output
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setMatrixInverse = setMatrixInverse,
    getMatrixInverse = getMatrixInverse
  )
}

# ------------------------------------------------------------------------------
# cacheSolve returns the inverse of a matrix as stored by the list object that
# is in turn created by calling the makeCacheMatrix function. This function will
# return the already computed and cached value of the inverse if one's
# previously computed for the same matrix and is readily available. If not it
# will calculate the inverse of the matrix and return it while also storing
# (caching) it for future reference, if needed.
# ------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  # retrieve the previously computed inverse of the matrix
  matrixInverse <- x$getMatrixInverse()
  
  # if the inverse was computed before and is available in cache
  # indicate that you are retrieving the inverse from the cache
  # and return the cached inverse value
  if(!is.null(matrixInverse)) {
    message("Getting Cached Data")
    return(matrixInverse)
  }
  
  # if cached inverse is not available
  # retrieve the matrix
  matrixData <- x$getMatrix()
  
  # compute the inverse of the matrix
  matrixInverse <- solve(matrixData)
  
  # cache the computed inverse
  x$setMatrixInverse(matrixInverse)
  
  # return the computed inverse
  matrixInverse
}
