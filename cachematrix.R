## Put comments here that give an overall description of what your
## functions do
##Brief Description:
##  Mat <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  matCache <- makeCacheMatrix(Mat) #matCache contains list of original & inverse matrix.
##  InMat <- cacheSolve(matCache)
##
##  matCache$set(Mat)      # Change the matrix being cached.
##  Mat <- matCache$get()  # Returns the matrix being cached.
##
##  matCache$setInverse(InMat) # Set the calculated inverse in cache
##  matCache$getInverse() #return invered matrix 

## Write a short comment describing this function
##This function generates setter and getter methods for matrix and inverse 
## of a matrix as we did in lecture. 

makeCacheMatrix <- function(x = matrix()) {
  inverseCache <- NULL
  
  ##Setter and getter for input matrix
  set <- function(y) {
    x <<- y
    inverseCache <<- NULL
  }
  get <- function() x
  
  ##Setter and getter for inverse of a matrix
  setInverse <- function(inverse) inverseCache <<- inverse
  getInverse <- function() inverseCache
  
#Return the object as a list.  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## I used 'solve' function to calculate the inverse of given matrix
##It will check inverse matrix null or not (if cached already)
#If it's null then cal it's inverse and set it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInverse()
  
  ##Inverse already calculated then print it and return it
  if(!is.null(matInv)) {
    message("getting cached inverse matrix")
    return(matInv)
  }
  
  ##Cal the inverse and set it in cache
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInverse(matInv)
  
  ##Return the inverse calclulated matrix
  matInv
}
