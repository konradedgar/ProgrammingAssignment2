## The function is submitted for the purpose of peer reviews assignment.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # ascribing null value to inverse x.
  inv.x <- NULL
  
  # the values of the special vector is set.
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  
  # the <<- is used to assign the value to inverse.
  setinv <- function(inverse) inv.x <<- inverse
  getinv <- function() inv.x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## The function reverses the matrix created through the previous function.
cacheSolve <- function(x, ...) {
  # take the getinv value.
  inv.x <- x$getinv()  
  
  # apply solve to return.      
  inv.x <- solve(x$get()) # get x values and solve.
  x$setinv(inv.x)
  
  # return reverse x.    
  return(inv.x)
}
}