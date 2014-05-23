## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an empty square matrix to allow caching the matrix
## when the matrix can be reused rather than calculated each time.

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
            x <<- y
            mx <<- NULL
      }

      get <- function() x
      setInverse <- function(solve) mx<<-solve
      getInverse <- function() mx
      list(set=set, get=get,
           setInverse=setInverse,
           getInverse=getInverse)
}

## Write a short comment describing this function
## This function calculates the inverse of a matrix to allow and then caches it
## rather than re-calculate the inverse each time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mx<-x$getInverse()
      if(!is.null(mx)) {
            message("getting cached data")
            return(mx)
      }
      
      data<-x$get()
      mx<-solve(data,...)
      x$setInverse(mx)
      mx
}
