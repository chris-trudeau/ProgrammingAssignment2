## makeCacheMatrix and cacheSolve are two functions that together
## will cache the inverse of the matrix that is given as an argument
## to the function when called.


## makeCacheMatrix defines the four functions required to set and get both the
## original matrices and their inverses. Finally, it returns the list object
## so that the functions are accessible for later use.

makeCacheMatrix <- function(x = matrix()) {
      m <- matrix(NULL)
      
      ## put the matrix into cache memory
      setMatrix <- function(y){
            x <<- y
            m <<- matrix(NULL)
      }
      
      ## retrieve the matrix from cache memory
      getMatrix <- function() x
      
      ## set the inverse matrix into cache
      setInverse <- function(solve) m <<- solve
      
      ## retrieve the inverse matrix from cache
      getInverse <- function() m
      
      ## return the list object with four matrices
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve first checks if there exists an inverse to the
## matrix provided as an argument to the function and, if not,
## calculates and caches that inverse for later retrieval

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse
      
      ## check if there is an inverse matrix already
      ## if yes, return it
      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      
      ## otherwise, calculate a inverse from the source matrix
      temp <- x$getMatrix 
      
      m <- solve(temp)
      x$setInverse(m)
      m
}