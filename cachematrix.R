## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

# Example usage:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = numeric()) {
   # s will store the cached inverse matrix
  s <- NULL
  
  #Setting matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #Getting matrix
  get <- function() x
  
  #Setting inverse matrix
  setsolve <- function(solve) s <<- solve
  
  #Getting inverse matrix
  getsolve <- function() s
  
  #Returning newly defined functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
       s <- x$getsolve()
       
   # If the inverse is already calculated, return it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #If not calculated already, calculate
  data <- x$get()
  s <- solve(data, ...)
  
  #Cache the inverse matrix
  x$setsolve(s)
  
  #Return
  s 
}
