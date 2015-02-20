## The functions in this R file are adapted from the "makeVector" and
## "cachemean" functions described in the Programming Assignment
## instructions.

## The makeCacheMatrix function:
##  1. Allows the user to create a matrix with the set function
##  2. Allows the user to get a matrix with the get function
##  3. Allows the user to set the inverse of a matrix with the
##     setinverse function
##  4. Allows the user to get the inverse of a matrix with the
##     getinverse function

makeCacheMatrix <- function(x = matrix()) {
  
  # Sets the value of i to NULL
  # Note: i stores the inverse of the matrix x
  i <- NULL
  
  # Stores the matrix x and sets i to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Retrives the stored matrix x
  get <- function() x
  
  # Sets the inverse of matrix x and stores it as i
  setinverse <- function(solve) i <<- solve
  
  # Retrives the inverse of the matrix x from i
  getinverse <- function() i
  
  # Compiles a list of the functions described above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function:
##  1. Returns the inverse of matrix x
##  2. Requires a matrix created using the makeCacheMatrix function
##  3. Computes the inverse of matrix x if the matrix is new or changed
##  4. Retrieves the cached inverse if it has been previously calculated

cacheSolve <- function(x, ...) {
  
  # Sets i to the value returned by the getinverse function in the
  # makeCacheMatrix function
  i <- x$getinverse()
  
  # Returns cached inverse (from makeCacheMatrix function) if the
  # getinverse function returns a non-null value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise retrives the matrix x using the get function in the
  # makeCacheMatrix function
  data <- x$get()
  
  # Computes the inverse
  i <- solve(data, ...)
  
  # Stores the inverse using the setinverse function in the
  # makeCacheMatrix function
  x$setinverse(i)
  
  # Returns the inverse
  i
}