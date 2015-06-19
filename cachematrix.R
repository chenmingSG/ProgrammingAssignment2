## The first function, makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function to: 
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the Matrix Inverse
## 4. get the value of the Matrix Inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function will first check whether the inverse already exist.
## if yes, it will directly return the inverse. 
## if no, it will calculate the inverse, and cache the inverse for future use. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}