## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" which is a list that contains four functions
# meant to either set or retrieve the original matrix or its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
                                   
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinv = setinverse,
       getinv = getinverse
       )
}


## Write a short comment describing this function
# This function takes in as an input the special "matrix" list, and then calculates its
# inverse and stores it in cache the first time. If the up to date inverse is cached already, 
# it returns the cached copy.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()

  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  orig <- x$get()
  inverse <- solve(orig, ...)
  x$setinv(inverse)
  inverse
}
