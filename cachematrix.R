## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function return a list of 4 functions in order to manipulate a special matrix. 
## the 4 function put/get the matrix and its inverse in mem

makeCacheMatrix <- function(x = matrix()) {
   # init the inverse of the matrix to NULL
   inv <- NULL
   
   # put the matrix in mem
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   # get the matrix from mem
   get <- function() x
   
   # put the inverse of the matrix in mem
   set_inv <- function(inverse) inv <<- inverse
   
   # get the inverse of the matri from mem
   get_inv <- function() inv
   
   # create a list with the function to create a special matrix in cache mem
   list(set = set, get = get,
        set_inv = set_inv,
        get_inv = get_inv)
}


## Write a short comment describing this function
## this function use a special matrix (a matrix and it's inverse) in order to do not
## calculate the inverse if it is already in mem

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
   # try to get the inverse of the matric in mem
   inv <- x$get_inv()
   
   if(!is.null(inv)) {
      # the inverse of the matrix is in mem,then return it
      message("getting cached data")
      return(inv)
   }
   # the inverse is not in mem, then calculate it
   # get the matrix in mem
   data <- x$get()
   
   # calculate the inverse
   inv <- solve(data, ...)
   
   # put the inverse in mem
   x$set_inv(inv)
   
   # return the inverse
   inv
}
