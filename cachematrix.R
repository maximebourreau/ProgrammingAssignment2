## Cache inverses of matrix

## Usage example
##
## Create a square matrix
## m <- matrix(c(2,6,9,3,7,10,4,8,1),nrow=3)
##
## Create an environnement for caching inverse of this matrix
## mCached <- makeCacheMatrix(m)
##
## First call to cacheSolve : compute inverse of m, and cache the result
## cacheSolve(mCached)
##
## Subsequent calls : retrieve cached inverse of m
## cacheSolve(mCached)


## Create an environnement around a matrix m in order to store its inverse
## and return a list of functions in order to get and set this matrix and its inverse

makeCacheMatrix <- function(m = matrix()) {
    solved <- NULL                 # Declare (but don't compute) the inverse of the matrix
    set <- function(pm) {
        m <<- pm                   # Change the matrix value
        solved <<- NULL            # Clear the cached inverse
    }
    get <- function() m            # Retrieve the matrix value
    setsolved <- function(psolved) solved <<- psolved
                                   # Set the inverse value
    getsolved <- function() solved
                                   # Get the inverse value
    list(
      set=set,
      get=get,
      setsolved=setsolved,
      getsolved=getsolved)
}

## From an matrix x created by makeCacheMatrix, return the inverse

cacheSolve <- function(x, ...) {
     solved <- x$getsolved()      # Check if already computed
     if(!is.null(solved)) {
        # The inverse has already been computed
                message("getting cached data")
                return(solved)
        }
        # The inverse has not yet been computed
        data <- x$get()            # Retrieved the initial matrix
        m <- solve(data, ...)      # Compute the inverse
        x$setsolved(m)             # Cache the inverse
        m                          # The inverse is returned
}
