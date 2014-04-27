## Function makeCacheMatrix creates a special matrix-like object
# that is capable of caching its inverse.
## Output of function makeCacheMatrix is a list of get and set functions:
## - setting the value of the matrix
## - getting the value of the matrix
## - setting the value of the inverse
## - getting the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # Proofing the matrix is square
  # otherwise, the function is going to be terminated
  if (nrow(x)!=ncol(x))
    { stop("The matrix is not square, therefore it is not invertible")} 
  # Initializing the stored inverse value as to be NULL
  inverse <- NULL
  # Setting the value
  set <- function(y)
    {
      x <<- y
      inverse <<- NULL
    }
  
  # Getting the value of the matrix
  get <- function() x
  # Setting the inverse
  setinv <- function(inv) { inverse <<- inv }
  # Setting the inverse
  getinv <- function() { inverse }
  
  # Returning all as a list
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}


## Function cacheSolve calculates the inverse of a
## matrix-like object created by the makeCacheMatrix function. 
## First, it checks whether the inverse was calculated already. 
## If it was, function cacheSolve just gets the inverse, omitting 
## computation stage. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse.

cacheSolve <- function(x, ...) {
  # Checking whether the inverse is already cached
  inverse <- x$getinv()
  if(!is.null(inverse))
    {
      message("getting cached data")
      return(inverse)
    }
  
  # Getting the matrix into data
  data <- x$get()
  # Computing the inverse
  inverse <- solve(data, ...)
  # Caching the inverse
  x$setinv(inverse)
  # Returning it
  inverse
}


# Testing the functions
m <- matrix(1:4, 2, 2) # example matrix
matrixCache <- makeCacheMatrix(m)


matrixInversedNotCached <- cacheSolve(matrixCache)
m %*% matrixInversedNotCached
matrixInversedCached <- cacheSolve(matrixCache) # cached inverse

# Making sure they are equal
identical(matrixInversedNotCached, matrixInversedNotCached)
