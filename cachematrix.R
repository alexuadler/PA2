makeMatrix <- function(x = matrix()) {
  ## A function is caled to cache the matrix
matInv<-NULL
# the variable that will hold the inverse matrix is cleared 

setMat<-function(y){  # This function seems to be for debugging; it isn't really called anywhere else 
  x <<- y
  matInv <<- NULL
  # the variable that will hold the inverse matrix is cleared
}

getMat<-function() x
# 'gets' the matrix from the makeMatrix function call

setInverse<-function(solve) {matInv<<-solve}
# solves (aka finds the inverse of) the matrix and 'sets' it in a safe place

getInverse<-function() matInv
# 'gets' the inverse matrix that was just solved above

list(setMat=setMat,
     getMat=getMat,
     setInverse=setInverse,
     getInverse=getInverse)
# store all of the setters and getters computed above to be called by cacheInverse()
}

cacheInverse <- function(x, ...) {
  matInv<-x$getInverse() # stores the inverse matrix in matInv (if it exists)
  
  if(!is.null(matInv)){ # if the inverse matrix is cached, this if statement tells the user it is being retreived from the cache
    message("retreiving cached matrix...")
    return(matInv) #returns the cached inverse matrix
  }
  
  # the code below calculates the inverse matrix if it does not exist in the cache
  myMat<-x$getMat() # loads the original matrix from the list created in makeMatrix
  matInv<-solve(myMat, ...) # stores the inverse of the matrix to matInv
  x$setInverse(matInv) 
  matInv # returns the inverse matrix
}
