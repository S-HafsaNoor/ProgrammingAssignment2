## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ## Initialize a cache
  cache <- NULL
  
  ## Define a function to set the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## Define a function to get the matrix
  get <- function() x
  
  ## Define a function to set the inverse of the matrix to the cache
  setinv <- function(inv) cache <<- inv
  
  ## Define a function to get the inverse of the matrix from the cache
  getinv <- function() cache
  
  ## Return a list with the four functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
      ## If the inverse has already been calculated (and the matrix has not changed), 
      ## then the cachesolve should retrieve the inverse from the cache.


        ## Return a matrix that is the inverse of 'x'
        ## Get the cached inverse if it exists
        inv <- x$getinv()
        
        ## Return the cached inverse if it exists
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        
        ## Otherwise, compute the inverse and set it to the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        ## Return the inverse
        inv
}
