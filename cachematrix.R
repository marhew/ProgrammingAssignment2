## Cache the inverse of a matrix to save computation time

## makeCacheMatrix given a matrix returns a set of functions 
## operating on the given matrix, allowing for caching
makeCacheMatrix <- function(theMatrix = matrix()) {
  inverse <- NULL
  setMatrix <- function(otherMatrix) {
    theMatrix <<- otherMatrix
    inverse <<- NULL
  }
  getMatrix <- function() theMatrix
  setInverse <- function(theInverse) {
    inverse <<- theInverse
  }
  getInverse <- function() inverse
  return(list(setMatrix = setMatrix,
              getMatrix = getMatrix,
              setInverse = setInverse,
              getInverse = getInverse)) 
}


## Given a cached matrix created with makeCacheMatrix
## returns its inverse re-using cached value if set, sets it otherwise
cacheSolve <- function(cachedMatrix, ...) {
  inverse <- cachedMatrix$getInverse()
  if (is.null(inverse)) { # if not cached: compute and cache inverse
    inverse <- solve(cachedMatrix$getMatrix()) # assume matrix is invertible
    cachedMatrix$setInverse(inverse) # cache inverse of matrix 
  }
  ## return the inverse of 'cachedMatrix'
  return(inverse)
}
