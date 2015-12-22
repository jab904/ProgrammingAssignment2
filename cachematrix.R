## These functions work as a pair to calculate the inverse of a matrix
## (assumed to be a square invertable matrix)
## and then caches the results to be used in the future
## so that the inverse of the matrix does not need to be recalculated 
## each time that it is needed.

## The makeCacheMatrix function creates a special "matrix" 
## which returns a list of functions:
## 1. Set, which can be used to set the matrix
## 2. Get, which retrieves the matrix
## 3. SetInverse, which sets the inverse of the matrix
## 4. GetInverse, which retreives the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL ##initialize holder variable for the inverse to be empty
  
  ##create function that can be used to define the matrix
  set <- function(y) { 
    x<<-y  ##set the new matrix to the imput of y
    i<<-NULL ##initializes i to be empty
  }
  
  ##create function that returns the matrix
  get <- function() x
  
  ##create fucntion that returns what is store in  i
  getInverse <- function() i
  
  ##create function that sets i
  ##to be equal to the inverse of the matrix
  setInverse<-function(inverse) i<<-inverse
  
  ##return a list of the functions created 
  return(list(set=set, get=get, 
              setInverse=setInverse, 
              getInverse=getInverse))

}


## The cacheSolve function calculates the inverse of the matrix that was created
## with the makeCacheMatrix function. It checks if the mean has already
## been calculated and stored, if so it retrieves those results.
## If the inverse has not yet been calculated this function will calculate
## it and store it for future use.

cacheSolve <- function(x, ...) {
  
  ## call getInverse to see if i has been assigned
  inverse<-x$getInverse()
  
  ## if i is not empty returned cached inverse results
  if (!is.null(inverse)) {
    print("Retrieving cached results")
    return(inverse)  ## Return a matrix that is the inverse of 'x'
  }
  
  matrix<-x$get()  ##if i is empty retrieve matrix data
  inverse<-solve(matrix,...) ## calculate inverse
  x$setInverse(inverse) ## stores the inverse in i
  return(inverse) ##returns the inverse of the matrix
        
}





