## Find inverse of a matrix if it has not been done previously

## Function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #Set Function: Set x=y and i=null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Get Function: To get x
  get <- function() x
  #Set Inverse Function: To set i = inverse (where function requires inverse to be given)
  setinverse <- function(inverse) i <<- inverse
  #Get Inverse Function: Print i (set as = inverse in setinverse)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check if it has already been done, then return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  #Check if already done
  i <- x$getinverse()
  if(!is.null(i)){
    return(i)
  }
  #Get the matrix
  matrix <- x$get()
  #Solve for inverse
  i <- solve(matrix, ...)
  #Set value of inverse
  x$setinverse(i)
  i   
}

##Test
test <- makeCacheMatrix(matrix(1:4,2,2))
test
test$get()
cacheSolve(test)
cacheSolve(test)
test$getinverse()
