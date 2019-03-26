## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## THIS FUNCTION WILL TAKE A MATRIX AS AN INPUT , SET AND THEN GET VALUES FOR THE MATRIX, SET AND GET THE INVERSE MATRIX.
## <<- WILL ASSIGN VALUES TO THE OBJECT WHEN THE ENVIRONMENT IS DIFFERENT FROM CURRENT ENVIRONMENT

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <-NULL
  setMatrix <-function(y){
    x<<-y
    invMatrix<<-NULL
    
  }
  getMatrix<-function()x
  setInverse<-function(inverse) invMatrix<<-inverse
  getInverse<-function() invMatrix
  list(setMatrix=setMatrix, getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##cacheSolve FUNCTION TAKES THE OUTPUT OF THE makeCacheMatrix AND CHECKS IF THERE EXISTS A VALUE FOR INVERSE MATRIX
## IF INVERSE MATRIX HAS SOME VALUE THEN IT RETURNS THE CACHED MATRIX ELSE IT GETS ORIGINAL MATRIX DATA AND SETS THE INVERTIBLE MATRIX BY SOLVE FUNCTION


cacheSolve <- function(x, ...) {
  
  invMatrix<- x$getInverse()
  if(!is.null(invMatrix)){
    return(invMatrix)
  }
  matrixdata<-x$getMatrix()
  invMatrix<-solve(matrixdata,...)
  x$setInverse(invMatrix)
  return(invMatrix)
  ## Return a matrix that is the inverse of 'x'
}
## end
