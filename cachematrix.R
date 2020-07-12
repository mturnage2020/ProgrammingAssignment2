## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(n) {
  #n is a determine the size of the square matrix... n x n
  
  #To run this function
  #inv<-makeCacheMatrix(n) ... where you select a number 2 or higher
  #inv is just a generic storage space for the result
  #example: >inv<-makeCacheMatrix(4)

    x<-sample(1:100,n*n)  #generate a vector with n*n random numers
  
  y<-as.numeric(x)      #convert random number integer vector to numerics
  
  special_matrix<-matrix(y,nrow=n,ncol=n) #create a n*n matrix
  print("Special Matrix!")
  print(special_matrix)
  
  inv_m<<-solve(special_matrix) #compute the inverse of the special matrix
  print("Inverse of the Special Matrix!")
  print(inv_m)
  
  m <- NULL
  setmatrix <- function(...) { #set the value of the matrix
    inv_m <<- m
    m <<- NULL
  }
  getmatrix <- function() inv_m #get the matrix value
  setinverse <- function(inv_m) m <<- inv_m #set the inverse matrix value
  getinverse <- function() m  #get the inverse matrix value
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix

#To run this function
#>cacheSolve(inv) where inv is the variable used to store the output 
#of makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

   m <- x$getinverse() #get the inverse value
  if(!is.null(m)) {
    message("getting cached data")
    print("Special Matrix!")
    return(m) #return the cached version of the matrix calculation
  }
  data <- x$getmatrix() #get the matrix value
  
  m <- solve(data, ...) #get the inverse matrix .. the original matrix
  
  x$setinverse(m)
  print("Special Matrix!")
  m
}
