## Author: Dal Marsters
## makeCacheMatrix returns a list of methods enabling you to
## makeMatrix - pass in a matrix and store it in an environment variable
## getMatrix - retrieves the stored matrix
## cacheSolve - calculates and returns the inverse of the stored matrix 
## or calculates the inverse a new matrix and stores it

makeCacheMatrix <- function(outside_matrix = matrix()) {
  makeMatrix <- function(given_matrix) {
    if (identical(given_matrix, outside_matrix)){
      #if the passed in matrix == the saved matrix, do nothing
       print("true")
    } else {
      #make the passed matrix the saved matrix
        outside_matrix <<- given_matrix
        #empty out the stored inverse
        inverse_cache <<- NULL
    }
  }
  
  #getMatrix retrieves outside scope x 
  getMatrix <- function() outside_matrix
  
  #if inverse exists, return it
  #else calculate new inverse, store it, and return it
  cacheSolve <- function(){
    if (!is.null(inverse_cache)){                   # if an inverse exists, 
        return(inverse_cache) 						# return inverse
    } else {                                      
        inverse_cache <<- solve(outside_matrix)   	# calculate and store inverse
        return(inverse_cache) 						# return stored inverse
    }
  }

  #returns list of methods
  list(makeMatrix = makeMatrix, getMatrix = getMatrix, cacheSolve = cacheSolve)
}