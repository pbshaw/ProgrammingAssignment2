##
## The following two functions will calculate the inverse of a matrix, caching the result
##
## When the inverse is requested for a matix whose inverse has been previously cached
## the cached result is returned without having to reclculate the inverse
##

##
## makeCacheMatrix 
##
## Description -
##   Stores a matrix and its inverse. Returns a list object
##   contining the data and functions required by the cacheSolve
##   function.
##
## Usage 
##   makeCacheMatrix(x)
##
## Arguments
##   x a numeric matrix
##
 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # variable inverse stores the inverse of the argument x (matrix)
                  # it is initialized to null when makeCacheAMtrix is called (creating a new object)
  # set function 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get function
  get <- function() { x }
  
  #setinverse function
  setinverse <- function(argInverse) { inverse <<- argInverse }
  
  # getinverse function
  getinverse <- function() { inverse }
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##
## cacheSolve 
##
## Description -
##   Returns the inverse of a matrix contined in the list object
##   passed as an argument. Will either return a previously cached
##   result or calculate the inverse and store the result back on the list object
##
## Usage 
##   cacheSolve(x)
##
## Arguments
##   x a list created by the makeCacheMatrix function
##
 
cacheSolve <- function(x, ...) {
  ## Function returns a matrix that is the inverse of 'x'
  ## Where x is a list object created by calling makeCacheMatrix function
  
  # call getInverse on list object
  inverse <- x$getinverse()
  
  # if result is null, the inverse must be calculated and cached before being returned
  # (1) obtain the matrix stored in makeCacheMatrix
  # (2) calculate the inverse using solve function
  # (3) storing the result back on makeCacheMatrix (caching it for future use)
  
  if(is.null(inverse)) {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
  }
  # if inverse is not null it has been previously cached
  # and can simply be returned
  else
  {
    message("getting cached data")
  }
  
  #return the inverse
  inverse
  
}
