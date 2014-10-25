## makeCacheMatrix function inputs a matrix, calculate the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(ginv) m <<- ginv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns cached matrix inverse using previously computed matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
             message("getting cached data")
             return(m)
         }
       data <- x$get()
       m <- ginv(data, ...)
       x$setinverse(m)
       m
}
