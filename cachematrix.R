## These functions will be used to cache the inverse of a matrix

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # intitialize inverse property
  m <- NULL
  
  ##set matrix
  set <- function (matrix) {
    x <<- matrix
    m <<- NULL
  }
  
  ## return matrix
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function()m
  
  ##return a list of methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## access the matrix  and get the value of the inverse
        i <- x$getinverse()
        
        ## check if inverse was already cached, send message and return the mean
        if (!is.null(i)) {
          message("getting cached data")
          return (i)
        }
        ## if x$getinverse() returned null, then calculate the inverse
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        ## return the inverse
        i
}
