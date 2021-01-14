## Set the function x as a matrix
## and then set the inverse value "t" as a null
makeCacheMatrix <- function(x = matrix(sample(1:50,6),1,1)) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function() t
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##
## changed "mean" to "inverse" and the variable "m" to "t"
cacheinverse <- function(x, ...) {
  t <- x$getinverse()
  if(!is.null(t)) {
    message("getting inversed matrix")
    return(t)
  }
  data <- x$get()
  t <- inverse(data, ...)
  x$setinverse(t)
  t
}