## Put comments here that give an overall description of what your
## functions do
# -- DESCRIPTION --
# makeCacheMatrix 
#  - creates an object with four function elements: set, get, setinv and getinv
#  - you can give the function a matrix argument
#    if no argument given, it takes an empty matrix as default
# cacheSolve 
#  - computes the inverse of an object created with makeCacheMatrix
#  - prints "getting cached data" if the inverse was already computed before
# -- EXAMPLE WALKTHROUGH --
# 1) create object 'matr' containing a 2x2 matrix with 1:4 elements ordered column-first
# matr <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# 2) check if 'matr' indeed contains a 2x2 matrix
# matr$get()
# 3) compute inverse of 'matr' with cacheSolve; prints the inverse
# cacheSolve(matr)
# 4) execute cacheSolve again on 'matr' to see if it prints "getting cached data"
# cacheSolve(matr)

## Write a short comment describing this function
# returns list with four functions:
# - set() sets the matrix 'x'
# - get() returns matrix 'x'
# - setinv() sets the inverse 'i' of matrix 'x'
#   NB can be set efficiently with cachSolve()
# - getinv() returns the inverse 'i' of matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# returns a matrix that is the inverse of 'x'
# 53) attempts to get inverse 'i' from 'x'
# 54) checks if 'i' already exists (is not null); 
#    if so, then 55-56) print "getting cached data", return 'í' and exit function
#    if not, then 
#       58) get the 'data' (matrix) from 'x'
#       59) compute inverse 'i' of 'data'
#       60) set inverse of 'x' to inverse 'i'
#       61) print inverse 'i' and exit function
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}