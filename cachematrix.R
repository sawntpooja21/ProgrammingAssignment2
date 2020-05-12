## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function containing input x as matrix to cache the inverse of matrix.
# initialized m as null at first in inverse
# set function set the value of vector 
# get function gets the value
# set_inverse function set the inverse 
# get_inverse function to get the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  # print(set_inverse)
  get_inverse <- function() m
  # print(get_inverse)
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Write a short comment describing this function
# This functions returns the inverse of function with the help of value returned by makeCacheMatrix.
# It will first check the condition if inverse is calculated through get_inverse of x and returns returns m.
# If not it will solve the problem and gets the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
