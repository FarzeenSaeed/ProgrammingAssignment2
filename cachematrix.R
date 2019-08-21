##  Below are two functions makeCacheMatrix and CacheSolve that are used to create a special matrix object and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which return a list containing functions to

## set the matrix
## get the matrix
## set the inverse of matrix
## get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  

}


## The following function calculates the inverse of the original matrix created with the above function makeCacheMatrix.However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
  
         ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  inv
  
  
}



