makeCacheMatrix <- function(x = matrix()) { ## define the argument with "matrix" default mode 
inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
set <- function(y) {                    ## define the set function to assign new 
     x <<- y                                ## value of matrix in parent environment
     inv <<- NULL                       ## if there is a new matrix, reset inv to NULL
   }
     get <- function() x                     ## define the get fucntion - returns value of the matrix argument
     setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
     getinverse <- function() inv                     ## gets the value of inv where called
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     }                                      ## you need this in order to refer 
                                              ## to the functions with the $ operator

## code below returns the inverse of the matrix by first checking if the inverse has already been computed
## if so, it gets the result and the function is complete
cacheSolve <- function(x, ...) {     
    inv <- x$getinverse()
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
      }                  ## If the inverse is not produced, it computes the inverse and sets the value in the cache as the 'setinverse' function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
