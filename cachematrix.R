## These methods togther allow computation of the inverse of a matrix 
## (via solve()) and the caching of the results in RAM. This allows for
## fast recall of the inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## Generates a list based object with getters and setters for storing a cached matrix.
  ## Specifically meant for use with cacheSolve()
  ##
  ## Args:
  ##   x: matrix to be inverted by cacheSolve()
  ## Returns:
  ##   A list object that can cache results for cacheSolve.
  
  inverse <- NULL # inverse should be handled via getInverse and setInverse
  
  # init x, clear inverse
  # NOTE: <<- means we set x defined in environment
  set <- function(y) {
    x <<- y 
    inverse <<- NULL
  }
  
  # get the matrix to take the inverse of
  get <- function() { x }
  
  # set the inverse
  set.inverse <- function(inverse) { inverse <<- inverse } # NOTE: <<- will assign value to inverse declared above
  
  # get the inverse
  get.inverse <- function() { inverse }
  
  # combine above methods and return it all as a list, which we use as a struct
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


cacheSolve <- function(x, ...) {
  ## Computes a matrix inverse and caches its value in a makeCacheMatrix list.
  ## Usage: 
  ##  >> cache <- makeCacheMatrix(matrix.to.solve)
  ##  >> cacheSolve(cache) # initial call to solve
  ##  >> cacheSolve(cache) # 2nd call, will access cached value
  ##
  ## Args:
  ##   x: list object created by makeCacheMatrix(mat) where mat is the matrix
  ##      that will be passed to solve()
  ##   ...: additional arguments passed to solve(mat,...)
  ##
  ## Returns:
  ##   The inverse of 'x'
  
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}
