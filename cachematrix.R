## OVERALL DESCRIPTION

# makeCachematrix creates a special "matrix" object that can cache its inverse, which enables
# cacheSolve to retrieve the inverse if it already exists in the cached data or to compute
# the inverse special "matrix".

## MAKECACHEMATRIX DESCRIPTION

# The first function, makeCacheMatrix creates a special
# "matrix" object that can cache the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix())
  {
## INITIALISATION
  inv <- NULL
  y <- NULL
 
  set <- function(y)
    {
    
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CACHESOLVE DESCRIPTION

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...)
  {
  inv <- x$getinverse()
  if(!is.null(inv))
    {
    message("an inverse has already been calculated")
 #   if(identical(x$set(x$get()),x$get()))
  #    {
      message("the matrix has not changed : getting cached data")
      return(inv)
    }
    y <- x$get()
    x$set(y)
    inv <- solve(y, ...)
    x$setinverse(inv)
    inv
  #}
}