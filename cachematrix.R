## These functions are used to calculate the inverse of a matrix.

## makeCacheMatrix() has four functions inside it.
##              set(x) is used to set x as the matrix for further calculations
##              get() is used to return the matrix if available, else it returns NULL
##              setInvertedMatrix(x) is used to set x as the inverse of the matrix for further calculations
##              getInvertedMatrix(x) is used to return x as the inverse of the matrix, if available, else it will return NULL

makeCacheMatrix <- function(matrix.data = matrix()) {
  
  invertedMatrix <- NULL
  
  set <- function(data){
    matrix.data    <<- data
    invertedMatrix <<- NULL
  }
  
  get <- function(){
    matrix.data
  }
  
  setInvertedMatrix <- function(inverseM){
    invertedMatrix  <<- inverseM
  }
  
  getInvertedMatrix <- function(){
    invertedMatrix
  }
  
  list(set = set, 
       get = get, 
       setInvertedMatrix = setInvertedMatrix, 
       getInvertedMatrix = getInvertedMatrix)
}


## This function first checks for the presence of the inverted matrix from a previous calculation.
## If the inverse is already present, then a message is displayed and the inverted matrix is returned.
## If the inverse is NOT present, then the matrix data is retrieved. Using solve(), the inverted matrix is calculated.
## After finding the inverse, the inverted matrix is set in the function above so that we can use it for further calculations.
## The inverse is finally returned.

cacheSolve <- function(matrix.data, ...) {

  invertedMatrix <- matrix.data$getInvertedMatrix()
  
  if(!is.null(invertedMatrix)){
    message("Retrieving Cahced Data.... Please wait.....")
    return(invertedMatrix)
  }
  
  data <- matrix.data$get()
  invertedMatrix <- solve(data, ...)
  matrix.data$setInvertedMatrix(invertedMatrix)
  invertedMatrix
}
