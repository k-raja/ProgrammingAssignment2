## This program is developed to satisfy the assignment requirement as part
## of Coursera R Programming course

## This program will cache inverse of a matrix which will eliminate the 
## costly repeated computation

## makeCacheMatrix will create a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) cachedInv <<- inverse
  getInverse <- function() cachedInv
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## cacheSolve function will return the inverse of the matrix. The function
## get the inverse from the cache (if available) rather than recomputing

cacheSolve <- function(x, ...) {
  cachedInv <- x$getInverse()
  if(!is.null(cachedInv)) {
    message("Gets inverse from cache if available (doesn't recalculate)!!!")
    return(cachedInv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}


## Sample Run output
## > source("C:/git/ProgrammingAssignment2/cachematrix.R")
## > test_matrix = makeCacheMatrix(matrix(1:4, 2, 2))
## > test_matrix$getInverse()
## NULL
## > test_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test_matrix$getInverse()
## NULL
## > cacheSolve(test_matrix)
## Error in cacheSolve(test_matrix) : attempt to apply non-function
## > source("C:/git/ProgrammingAssignment2/cachematrix.R")
## > test_matrix = makeCacheMatrix(matrix(1:4, 2, 2))
## > test_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test_matrix$getInverse()
## NULL
## > cacheSolve(test_matrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(test_matrix)
## Gets inverse from cache if available (doesn't recalculate)!!!
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 