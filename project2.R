##function1

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  get <- function() 
    x
  
  
  setinv <- function(solve) 
    inv <<- solve
  
  
  getinv <- function() 
    inv
  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##function2

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  
    }
  
  
  data <- x$get()
  inv <- solve(data) %*% data
  x$setinv(inv)
  inv
}