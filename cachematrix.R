# Creates special object that stores a array or matrix
makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y){
    x <<- y
    p <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) p <<- inverse
  getInverse <- function() p 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##Following function calculates the inverse of the special array created with the
##above function. However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)){
    message("getting cached data")
    return(p)
  }
  mat <- x$get()
  p <- solve(mat,...)
  x$setInverse(p)
  p
}