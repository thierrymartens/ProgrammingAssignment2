## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize the "inverse" matrix (to Null)
  inv <- NULL

  #return a message if the input is not a square matrix
  if ((ncol(x)!=nrow(x))) print ("this input is not a square matrix!")    

  
  #create function to a) input/change the matrix and b) initialize the inverse (to Null)
  #if the input does not change the matrix, nothing happens
  #if the input is not invertible, nothing happens (original matrix is maintained, inverse as well)
  set <- function(y = matrix()) {

    if (identical(x,y)) return ("this input is already in! no changes are necessary!")

    if (ncol(y)==nrow(y))  {  #check whether this is a square matrix....
      if (det(y)!=0) {  #... and whether the determinant <> 0 (a sufficient condition for inversion)
        x <<- y
        inv <<- NULL
      }
    } 
    else return ("this input is not an invertible matrix! no changes are made!")
      
  }
  
  
  #create a function to read the matrix
  get <- function() x
  
  
  #create a function to set the inverse of the matrix
  setinverse <- function(inverse=matrix()) {

    #check whether the "inverse" is indeed the inverse! 
    #(this function could be called from the console...)
    mat<-get()
    if (nrow(inverse)==ncol(inverse) & ncol(mat)==nrow(inverse)) {
     if (isTRUE(all.equal(mat%*%inverse,diag(nrow(mat))))) inv <<- inverse
    } 
     else {
      inv<<- NULL
      return("this is not the inverse matrix! use cacheSolve to calculate & store the inverse!")
    }  
  }
  
  
  #create a function to call/look up the inverse matrix
  #inv is a variable that only exists "inside the function"
  getinverse <- function()    inv
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check whether matrix is square
  if (ncol(x$get())!=nrow(x$get())) return ("matrix is not square!")
  
  #check whether matrix is invertible
  if (det(x$get())==0) return ("matrix is not invertible!")

  inverse <- x$getinverse() #use the getinverse function to check whether the inverse 
                            #has already been calculated and stored
                            #the setinverse function has been written so that 
                            #a stored non-null matrix is indeed the real inverse!

  #if the inverse has already been stored
  if(!is.null(inverse))   {
        message("getting cached data")
        return(inverse) 
  }

  #if the inverse has not been calculated.... 
  data <- x$get() #use the get function to "import" the matrix
  inverse <- solve(data, ...) #use the solve function to calculate the inverse of the matrix
  x$setinverse(inverse) #use the setinverse function to store the inverse (it will be double-checked)
  inverse #returns the inverse to the console        
}
