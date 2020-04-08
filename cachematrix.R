## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## Created functions provide caching the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inversedMatrix <- NULL 
  
  set <-function(y) { 
    x <<- y 
    inversedMatrix <<- NULL 
  } 
  
  
  get <- function() x 
  setInversedMatrix <- function(iMx) inversedMatrix <<- iMx 
  getInversedMatrix <- function() inversedMatrix 
  list(set = set, get = get, setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix) 
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##  If the inverse has already been calculated (and the matrix has not changed), then the 
##  cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) { 
           ## Return a matrix that is the inverse of 'x' 
     inversed <- x$getInversedMatrix() 
      
   if(!is.null(inversed)) { 
     message("getting cached data") 
     return(inversed) 
   } 
    
   data <- x$get() 
   inversed <- solve(data, ...)  # solve function create inverse of a square matrix
                                
   x$setInversed(inversed) 
    
   inversed 
} 


# Example

# z <- matrix(c(8,5,13,8),nrow=2,ncol=2) 
# cs <- makeCacheMatrix(z)
# cacheSolve(cs)
