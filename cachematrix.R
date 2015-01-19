
## The following two functions serve to cashe potentially time-consuming computations of matrix's innverse.
## For a very big matrix, it may take long to compute the inverse. 
## If the content of a matrix is not changing, it make sense to cache the value of the inverse.
## If needed again, it can be looked up in the cache rather than recomputed. 


# This function creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix's inverse
# get the value of the matrix's inverse

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


## This function computes the inverse of the special matrix" returned by `makeCacheMatrix` above. If the inverse has
## If already been calculated (and the matrix has not changed), then the
## `cacheSolve` retrieves the inverse from the cache.

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
