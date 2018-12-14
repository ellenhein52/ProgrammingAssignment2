## The following functions create a list of functions 
## (that act like a class) that cache the inverse of 
## a matrix.

## Inputs: a matrix, default matrix()
## Creates a matrix that caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Assumes the matrix is invertable
  i <- NULL  # Initialize inverse
  set <- function(y){
    x <<- y     # Initialize matrix
    i <<- NULL  # Initialize inverse
  }
  get <- function() x  # Function to initialize matrix
  setinverse <- function(solve) i <<- solve  # Function to set the inverse
  getinverse <- function() i  # Function to get the inverse
  
  # List of functions (kind of like a class)
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Inputs: a list created by makeCacheMatrix
## Outputs: matrix
## Calculates and caches the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()  # Get the inverse of the matrix
  
  # Check if the inverse already exist,
  # if so, return the cached value
  if(!is.null(i)){
    message("getting cached data")
    return(i)  # Return the cached inverse
  }
  
  # Runs only if i is NULL
  data <- x$get()  # Get the matrix
  i <- solve(data, ...)  # Run solve to calculate the inverse of the matrix
  x$setinverse(i) # Set the inverse of the matrix
  return(i)  # Return the inverse
}
