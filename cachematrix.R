## Cache optimized calculation for the inverse of the matrix ##

## Special Matrix Constructor
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## inverse of the matrix attribute
  
  ## sets the new matrix and resets the cached inverse
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## returns the matrix
  get <- function() inv
  
  ## sets the current inverse of the matrix
  setinv <- function(i) inv <<- i
  
  ## gets the currente inverse of the matrix
  getinv <- function() inv
  
  ## subfunctions list
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Cached optimized function to calculate the inverse of a matrix 
  ## Parameter x type: Special Matrix (makeCacheMatrix)
cacheSolve <- function(x, ...) {
  c_inv <- x$getinv() ## Gets the cached inverse
  if(!is.null(c_inv)){
    ## The inverse has been calculated previosly
    message("getting cached data")
    return(c_inv)
  }
  ## The inverse hasn't been calculated yet
  data <- x$get()
  c_inv <- solve(data)
  x$setinv(c_inv)
  i
}
