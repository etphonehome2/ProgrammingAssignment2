##  Overview:
##  --------
##  Matrix inversion is usually a costly computation and there 
##  may be some benefit to caching the inverse of a matrix rather 
##  than to compute it repeatedly. 
##
##  Usage Example:
##
##     r = rnorm(1000)
##     mat = matrix(r, nrow=10, ncol=10)
##     temp = makeCacheMatrix(mat)
##     start.time <- Sys.time()
##     capture.output( garbage <- cacheSolve(temp))
##     print(Sys.time() - start.time)
##
##


##  makeCacheMatrix Function Overview:
##  ---------------------------------
##  Creates a matrix object that caches its inverse.
##  Saves the matrix to variable x and its inverse to variable inv
##  The returned object is a vector list
##

makeCacheMatrix <- function(x = matrix()) {
  ## 
  ## Input:
  ##   x: input matrix must be a squared matrix, ie., ncols = nrows
  ##      this script DOES NOT validate if input is a square matrix
  ## 
  ## Output:
  ##   return: a vector list pointing to:
  ##      1. set the matrix [set=]
  ##      2. return the matrix [get=]
  ##      3. set the inverse [setinv=]
  ##      4. return the inverse [getinv=] cache value
  ##
  
  inv = NULL
  set = function(y) {
    ## assign value to an object in an environment different
    ## from current environment
    x <<- y
    inv <<- NULL
  }
  
  get    = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}




##  cacheSolve Function Overview:
##  ----------------------------
##  Get the inversed matrix from created by makeCacheMatrix.
##  Determine if the object argument 'x' inverse is cached:
##    if already cached - returns the cached value
##    if not cached - this function calculates the inverse, 
##       and it into 'x' cache calling 'setinv', and returns
##       the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##
  ## Input:
  ##   x: output of makeCacheMatrix()
  ##
  ## Ouput:
  ##   inv: inverse of the original matrix
  ##
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data = x$get()
  inv  = solve(data, ...)
  # print statements used for validation matrix format correct
  # print(data)           # print matrix input
  # print(inv)            # print inverted matrix
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
