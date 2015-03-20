## makeCacheMatrix is a function that takes a matrix x as its
## argument, and returns a list of four functions:-
##   1) setmatrix - takes a matrix argument, sets the value of x to it, and
##                  initiatilizes its cached inverse matrix invmatrix to NULL   
##   2) getmatrix - returns the matrix x
##   3) setinverse - assigns the cached inverse matrix  invmatrix its argument
##   4) getinverse - returns the cached inverse matrix invmatrix do


makeCacheMatrix <- function(x = matrix()) {
      invmatrix <- NULL
      setmatrix <- function(a_matrix) {
            x <<- a_matrix
            invmatrix <<- NULL
      }
      getmatrix<- function() x
      setinverse <- function(inversematrix) invmatrix <<- inversematrix
      getinverse <- function() invmatrix
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of its argument
## If the inverse has already been computed and cached, it returns cached value
## Otherwise it calculates the inverse through solve and returns it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invmatrix <- x$getinverse()
      if (!is.null(invmatrix)) {
            message("getting cached inverse of matrix")
            return(invmatrix)
      }
      matrix1 <- x$getmatrix()
      invmatrix <- solve(matrix1,...)
      x$setinverse(invmatrix)
      return(invmatrix)
}

## What I used to test the functions after sourcing file
## > matrix1=matrix(1:4,2,2)
## > matrixmCM1=makeCacheMatrix(matrix1)
## > invmatrix1=cacheSolve(matrixmCM1)
## The above produces the inverse of matrix1
## > invmatrix1
## Confirms this
## > matrix1%*%invmatrix1  
## The above will produce the identity matrix
## > cacheSolve(matrixmCM1)
## The above produces the message "getting cached inverse of matrix"

