# Put comments here that give an overall description of functions
##makeCacheMatrix creates a cached inverse of a matrix for fast comp. later
##solveCache calls the matrix inverse from a cache if available or recalcs

# Write a short comment describing this function
##makeCacheMatrix creates a cached inverse of a matrix for fast comp. later

makeCacheMatrix <- function(x = matrix()) {
   #create local variable for the matrix inverse i
   m <- NULL
   
   #create global variables for the matrix and its inverse
   set <- function(y) {
      x <<- y 
      m <<- NULL 
   }
   #create cached inverse
   setinverse <- function(solve) m <<- setinverse
   
   #create variables to return matrix and its cached inverse
   get <- function() x
   getinverse <- function() m
   
   #return a list with calculated values (not stored in this form)
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
##solveCache calls the matrix inverse from a cache if available or recalcs

cacheSolve <- function(x, ...) {
   ##Return a matrix that is the inverse of x from a cache
   m <- x$getinverse()
   
   if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
   }
   
   ##Recalculate inverse, store, and return a matrix that is the inverse of x
   ##Calc will not reach here unless the matrix changes
   
   data <- x$get()
   m <- solve(data)
   x$setinverse(m)
   m
   
   ##if interested, test speed difference between cache and non-cache options
   ##need to install the microbenchmark package before executing
   ##use microbenchmark(m,solve(m)) to test
   
}
