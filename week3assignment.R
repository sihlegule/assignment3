makeCacheMatrix <- function(x = matrix()) {
    invrse <- NULL                           
    set <- function(y) {                    
    x <<- y                             
    invrse <<- NULL                      
  }
get <- function() x                     
setinverse <- function(inverse) invrse <<- inverse  
getinverse <- function() invrse                     
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)




cacheSolve <- function(x, ...) {
  invrse <- x$getinverse()
  if(!is.null(invrse)) {
    message("getting cached data")
    return(invrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinverse(invrse)
  invrse
}