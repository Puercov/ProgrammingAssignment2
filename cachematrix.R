## The overall idea is to create a special "matrix" to cache the inverse of a matrix
## so that we can access to this value without repeating the calculation every time.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## 2.cacheSolve: This function computes the inverse of the special "matrix". 
#If the inverse has already been cached, It is directly retrieved.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

## 3.Example of use

cm<-makeCacheMatrix(matrix(c(1,2,5,3),2,2))
cm$get() # retrieve the value of the matrix defined above
cm$getinv() # retrieve the value of the inverse of the matrix, NULL so far
cm$set(matrix(c(2,4,2,3),2,2)) # redefine the value with a new matrix
cacheSolve(cm) # the function cacheSolve calculates the inverse for the matrix defined with cm$set
cm$getinv() # the inverse of the matrix has been cached and cm$getinv() retrieve it directly
