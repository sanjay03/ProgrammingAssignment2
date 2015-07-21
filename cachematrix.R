## Caching the Inverse of a Matrix

## makeCacheMatrix - this function creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  z<- NULL               ##setting placeholder for inverse of a matrix to NULL
  set <- function(y){    ## defines a function that sets a matrix to a new object y
    x <<- y
    z <<- NULL
  }
  
  get <- function() x     ## return the input matrix
  setinverse<- function(inverse) z <<- inverse   ## sets the inversed matrix
  getinverse <- function()z  ## returns the inversed matrix
  list(set=set, get=get,     ## returns a list containing functions defined above
       setinverse=setinverse,
       getinverse=getinverse)
  
}     



## cacheSolve - this function retrieves the inverse of the matrix from 
## cache or computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse() ##gets the inversed matrix from x
  if(!is.null(z)){    
    message ("getting cached inverse matrix")
    return(z)
  }
  data <- x$get()  ## not cached,getting the matrix from x
  z<- solve(data,...) ## computing the inverse 
  x$setinverse(z)  ## setting the inverse 
  z                ## returns the inversed matrix 
}
