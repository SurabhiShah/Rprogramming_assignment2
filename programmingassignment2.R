
##Making function name makecachematrix that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
##Initialize the inverse 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
##Method the get the matrix
get <- function() x
##Way to set the inverse of the matrix
setInverse <- function(inverse) inv <<- inverse
##Way to get the inverse of the matrix
getInverse <- function() inv
##Back a list of the methods
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) }


cacheSolve <- function(x, ...) {
##Return a matrix that is the inverse of 'x'
inv <- x$getInverse() 
if (!is.null(inv)) { message("getting cached data") 
  return(inv) } 
mat <- x$get()
##Compute the inverse via matrix multiplication
inv <- solve(mat, ...)
##Set the inverse to the object
x$setInverse(inv)
##Coming back the matrix
inv }

##example for Testing the function code 
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) 
my_matrix$get()
my_matrix$getInverse() 
cacheSolve(my_matrix) 
cacheSolve(my_matrix) 
my_matrix$getInverse()
