## The functions below enable the user to cache the inverse of a matrix, which is a time
## consuming calculation, so that when we need the inverse again it can be found in the
## cache rather than being recalculated.
## For the function to work it is necessary for the user to supply a invertible matrix!

#############################################


# The first function, makeCacheMatrix creates a special "matrix", which is really a 
# list containing a function to:
# set the values of the matrix
# get the values of the matrix
# set the values of the inverse matrix
# get the values of the inverse matrix


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

#################################################

# The function cacheSolve calculates the inverse of the matrix created with the
# makeCacheMatrix function.  However, it first checks if the inverse has already been
# calculated.  If the inverse has already been calculated the function retrieves 
# the inverse from the cache and doesn't recalculate the inverse, but if the inverse
# has not been calculated the function calculates the inverse of the matrix and then
# sets the values of the inverse matrix in the cache via the setinverse function (from above)




cacheSolve <- function(x, ...) {
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