## file: cachematrix.R
## Author: Bryan Wise
##
## makeCacheMatrix - creates an object to store a matrix and its inverse.  Four functions
## are included: set, get, setinv, getinv
##
## example usage:
## > my_matrix <- makeCacheMatrix()
## > my_matrix$set(matrix(1:4,2,2))
## > my_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_matrix$setinv(solve(matrix(1:4,2,2)))
## > my_matrix$getinv()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > my_matrix$get() %*% my_matrix$getinv()
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## cacheSolve - returns the cached inverse if it exists in the object
## if the cached inverse does not exist, it computes the inverse, stores the inverse
## in the cache and returns the cache
##
## example Usage:
## > cacheSolve(my_matrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > my_matrix$set(matrix(2:5,2,2))
## > cacheSolve(my_matrix)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
##

## makeCacheMatrix - creates an object to store a matrix and its inverse.  Four functions
## are included: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  invx <- matrix()
  set <- function(y) {
     x <<- y
     invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - returns the cached inverse if it exists in the object
## if the cached inverse does not exist, it computes the inverse, stores the inverse
## in the cache and returns the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##This checks to see if the inverse has been cached
  ##If inverse has not been cached, compute it and store in cache
  ##Note that if the matrix has been changed, the cache gets cleared automatically
  if(is.null(x$getinv())) {
    x$setinv(solve(x$get()))
  }

  
  ##This returns the inverse that is stored in cache
  x$getinv()
}
