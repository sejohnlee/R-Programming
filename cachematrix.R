## Combining the two ftns, makeCacheMatrix() and cacheSolve(), one can compute and cache 
## the inverse of a matrix and reuse the reverse.

## makeCacheMatrix() creates four ftns used to ...
## 1. cache and get a matrix object (the ftns of set and get)
## 2. cache and get its inverse (the ftns of setinvM and getinvM)
## Assume the matrix passed to ftns as arg is always invertible, 
##   or it should be test in the first place.

makeCacheMatrix <- function(x = matrix()) {
      ## The default value of X is a 1x1 matrix & X(1,1)=NA, not NULL
      ## Under 2 situations, data is stored as X in the environment 
      ## of an obj of makeCacheMatrix.
      ## 1. call the ftn of makeCacheMatrix(data); 2. call the ftn of $set(data)
      
      invX <- NULL            # As an obj of this ftn being created, invX is set to NULL
      set <- function(Y){
            X <<- Y           # Now, Y is stored in the defining environment of X
            invX <<- NULL     # As a new matrix is set, invX is reset to NULL at the same time.
            # invX is not defined in the current env of $set so <<- is used.
      }
      get <- function() X
      setinvM <- function(invM) invX <<- invM  #NOw, the value of invM is stored in the parent env
      getinvM <- function() invX
      list(set=set, get=get, setinvM=setinvM, getinvM=getinvM)
}

## cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, it will be retrieved from the cache.

cacheSolve <- function(x, ...) {
      ## x: an obj of makeCacheMatrix()  
      ## ...: arguments pass to solve()
      invX <- x$getinvM()           # Get inverse matrix
      if(!is.null(invX)) {          # NULL-> The inverse was not cached or X was reset.
            message("getting cached data")
            return(invX)
      }
      data <- x$get()               # Get the matrix
      if ((length(data)==1)&(is.na(data[1,1]))) {         # Check if any matrix is cached.
            message("No matrix needs to be inverted!")
            return()
      }
      invX <- solve(data, ...)      # Calculate the inverse matrix
      x$setinvM(invX)               # Set the inverse 
      invX
        ## Return a matrix that is the inverse of 'x'
}
