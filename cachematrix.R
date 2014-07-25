##  Creates a special "matrix" object that can cache its inverse.
##  x : the matrix to encapsulate
##  set(y) : set the matrix
##  get : get the matrix
##  setinverse(inverse) : set the inverse of the current matrix
##  getinverse : retrieve the cached inverse matrix or NULL if no inverse matrix was set

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
       x <<- y
       inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)   
}


## Compute the inverse matrix of the CacheMatrix x.
## This function returns the cached inverse matrix of x if it exists. 
## If not it computes the inverse matrix of the encapsulated matrix of x and caches the result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
