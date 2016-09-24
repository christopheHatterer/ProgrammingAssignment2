## Function cacheSolve returns a matrix that is the inverse of its argument 'x' (also a matrix).
## cacheSolve makes use of 4 functions created by function makeCachematrix. 

## Function makeCacheMatrix creates an object with an attached list of 4 functions. 
## These functions set and retrieve values of x and cachedMatrix. 
## Values of x and cachedMatrix are stored in the parent environement. 

makeCacheMatrix <- function(x = matrix()) {
      cachedMatrix <- NULL
      set <- function(y) {                  # Stores value y into x (parent env.)
            x <<- y
            cachedMatrix <<- NULL           # Resets cache
      }
      get <- function() x                   # Retrieves value of initial Matrix
      setinv <- function(inverseMatrix){    # Stores value of inverse matrix into cache
            cachedMatrix <<- inverseMatrix
      } 
      getinv <- function() cachedMatrix     # Retrieves value of inverse matrix from cache
      list(set = set,                       # Returns the list of 4 functions  
           get = get,           
           setinv = setinv,
           getinv = getinv
           )

}
           

## Function cacheSolve returns a matrix that is the inverse of its argument 'x'. 
## If the cache does not contain any value, cacheSolve calculate the inverse 
## of matrix x and store it in cache. If the cache contains a value, cacheSolve just 
## retrieves the value of the inverse matrix stored in the cache. 
## The inverse matrix is then displayed. 

cacheSolve <- function(x, ...) {          
        
      cachedInverse <- x$getinv()         # Retrieves cached matrix (if it exists) 
      if(!is.null(cachedInverse)) {       # If a matrix is stored in the cache then... 
            message("getting cached data")
            return(cachedInverse)         #... return this matrix and exit
      }
      data <- x$get()                     # Otherwise, retrieve value of initial matrix...   
      cachedInverse <- solve(data, ...)   # and invert it,...
      x$setinv(cachedInverse)             # then store it in cache ...
      cachedInverse                       # and return its value.
}
      
      
## Usage of functions

mm <- makeCacheMatrix()                   # Create object mm with list of functions  
mm$set(matrix(                            # Set value of initial matrix (here a 5X5 matrix)
      c(6, 2, -2, 1, 3, 1, 5, 2, 1, 4, 5, 2, 1, 0, 2, 5, 1, -1, 1, 4, 2, 3, 3, 1, 2),
      5, 
      5)) 
mm$get()                                  # Check value of initial matrix
cacheSolve(mm)                            # Determine inverse matrix and cache it
cacheSolve(mm)                            # Re-access inverse matrix (got from cache)
mm$get() %*% cacheSolve(mm)               # Check that initial X inverse = identity matrix
                                          # Notice message "getting cached data" when matrix...
                                          # is retrieved from  cache instead of being calculated.

