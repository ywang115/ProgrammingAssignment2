##Lexical Scoping Assignment##
##Yiru Wang##

##Function 1: create makeCacheMatrix
##Create matrix 
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get and return the matrix
  get <- function() {
    m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get and return the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##Function 2: cacheSolve as the inverse of the cache
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  x<- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(x)
  
  ## Return the matrix
  x
}
