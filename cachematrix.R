makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  
  ##              1. set the matrix
  
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  ##              2. get the matrix
  
  get = function() x
  
  ##              3. set the inverse
  
  setinv = function(inverse) inv <<- inverse 
  
  ##              4. get the inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  # Check if the inverse is already calculated
  if (!is.null(inv)){
    # If not null, get inverse from the cache and skips the computation. 
    message("Retriving cached data")
    return(inv)
  }
  
  # If inverse does not exist, calculate the inverse 
  matrix.data = x$get()
  inv = solve(matrix.data)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}