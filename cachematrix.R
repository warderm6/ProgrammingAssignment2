## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL # store the cached inverse matrix
  # Function 1: Set the matrix
  set <- function(y)
  {
    x <<- y
    inv_matrix <<- NULL
  }
  # Function 2: Get the matrix
  get <- function() x
  # Function 3: Set the inverse
  setinverse <- function(inverse) inv_matrix <<- inverse
  # Function 4: Get the inverse
  getinverse <- function() inv_matrix
  # List of 4 functions: set, get, setinverse, getinverse
  list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: Compute the inverse matrix if it has not been calculated before
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  
  if (!is.null(inv_matrix)) # inv_matrix different from null
  {
    message("getting cached data")
    return(inv_matrix)
  }
  # Calculation of inverse matrix
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix # return the inv_matrix
}
