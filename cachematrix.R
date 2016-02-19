## In this code are two functions:
## makeCacheMatrix is a function that create a special "matrix" object that can
## cache its inverse
## cacheSove is a function that compute the especial matrix returned by
## makeCacheMatrix.

## This is an example of use:
## Create a matrix special object
## > m <- makeCacheMatrix()
## set special matrix object
## > m$set(matrix(c(1,2,3,0,1,4,5,6,0),3,3))
## Compute matrix inverse
## > cacheSolve(m)
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## Call matrix inverse that return cached result
## > cacheSolve(m)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1

## makeCacheMatrix create special matrix objects. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve compute inverse matrix of a special objects and return cahed data
## when its computed

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) { 
      message("getting cached data")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
    inv
}
