## The objective of the functions in this R file are to take advantage
## of the lexical scoping rules of the R language by storing in  cache 
## the inverse value of a matrix; in order to avoid time consuming 
## computations. For example, when running loop functions that would 
## otherwise compute the inverse if the matrix several times until the
## end of the loop.


## MakeCacheMatrix is a function that works as a container of a list
## of four other sub functions under its code. the sub functions are:
## 1. setMatrix           
## 2. getMatrix           
## 3. setInverseMatrix    
## 4. getInverseMatrix

makeCacheMatrix <- function(x = matrix()) { 
  
  ## Cache is empty at the beginning
  i <- NULL     
  
  ## Matrix is stored and the cache is emptied 
  ## from previous from previous data.
  setMatrix <- function(y) { 
    
    x <<- y       
    
    i <<- NULL   
    
  }                           
  
  ## Returns value in cache for matrix.
  getMatrix <- function() x  
  
  ## Stores value for inverse matrix either through
  ## manual input or through solveCache.
  setInverseMatrix <- function (solve) i <<- solve
  
  ## Returns value in cache for inverse matrix.
  getInverseMatrix <- function () i
  
  ## List of functions under makeCacheMatrix.
  list( setMatrix = setMatrix,
        
        getMatrix = getMatrix,
        
        setInverseMatrix = setInverseMatrix,
        
        getInverseMatrix = getInverseMatrix)
  
} 


## The function cacheSolve returns a matrix that is the inverse of 'x',
## the one which has been stored in the function makeCacheMatrix by
## using setInverseMatrix. If there is no value for a matrix inverse
## 'x', then the function cacheSolve will calculate such value and
## proceed to storing the result in cache to avoid reiterative 
## computations.

cacheSolve <- function(x, ...) { 
  
  ## Reads the value in cache of the inverse matrix.
  i <- x$getInverseMatrix()
  
  ## Returns the value in cache of the inverse matrix.
  if(!is.null(i)) {
    
    message("getting cached data")
    
    return(i)
    
  }
  
  ## If there is no value in cache for the inverse
  ## matrix, then store the value of the matrix.
  data <- x$getMatrix()
  
  ## Assign computation of the inverse of the matrix.
  ## solve() will only compute the inverse of a matrix 
  ## for which function det() computes a determinant
  ## different from zero. For peer review, assume matrix
  ## is invertible.
  i <- solve(data, ...)
  
  ## Stores value of inverse matrix in cache.
  x$setInverseMatrix(i)
  
  ## Returns the computed value of the inverse.
  i
  
} 
