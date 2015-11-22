## function will cache the inverse of a matrix


## The first function, makeVector creates a special "vector", which is really a list containing a function to
  ##  set the value of the vector
  ## get the value of the vector
  ## set the value of the mean
  ##  get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
    library(MASS)
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    settranspose <- function(ginv) m <<- ginv
    gettranspose <- function() m
    list(set = set, get = get,
         settranspose = settranspose,
         gettranspose = gettranspose)
}



## Calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inversion in the cache via the settranspose function.

cacheSolve <- function(x, ...) {
    m <- x$gettranspose()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data, ...)
    x$settranspose(m)
    m
}
