## ----------------------------------------------------------------------------------------------------------
##  A closure is a function written by another function. Closures are so called because they enclose the 
##   environment of the parent function, and can access all variables and parameters in that function. This 
##   is useful because it allows us to have two levels of parameters. One level of parameters (the parent) 
##   controls how the function works. The other level (the child) does the work.   
##
##  The makeCahceMatrix function makes use of the superassignment operator '<<-'.  It does the assignment in the
##   enclosing environment. It starts with the enclosing frame, it works its way up towards the global 
##   environment until it finds a variable of the same name, and then assigns to it.   If it never finds
##   an existing varialbe with that name it creates one in the global environment.   This operator makes it possible to 
##   maintain 'state' across function invocations by allowing a function to modify variables in the environment 
##   of its parent.
## ----------------------------------------------------------------------------------------------------------

##  Each time makeCacheMatrix is run, it creates an environment, initializes the m variable in this environment, 
##   and then creates a list containing new functions. 
## ----------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## function to set the value of the matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }

  ## function to get the value of the matrix
  getMatrix <- function() x

  ## function to set the value of the matrix
  setInvMatrix <- function(inverse) inv <<- inverse

  ## function to get the value of the matrix 
  getInvMatrix <- function() inv

  ## build the list of functions
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## ----------------------------------------------------------------------------------------------------------
##  This function will compute the inverse of the given matrix and call a function to store it in cache.
##   If called again for the same matrix, the function should retreive the inverse from the cache (as opposed 
##   to re-computing the inverse
## ----------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

  ## attempt to retrieve the matrix inverse from the cache
  inv <- x$getInvMatrix()

  ## if the inverse matrix does exist, show message and return it to the caller
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## the inverse above was null , so we must get the matrix
  mt<- x$getMatrix()

  ## then compute the inverse of the matrix
  inv <- solve(mt, ...)

  ## then save the inverse iatrix to the cache
  x$setInvMatrix(inv)

  ## retrun the inverse matrix
  inv
}