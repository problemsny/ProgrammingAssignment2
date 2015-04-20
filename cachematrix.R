## This is for Coursera Assignment #2
## The ability to cache a matrix - To return the cache if it exists.

## makeCasheMatrix - Matrix to cache matrix inversions.

makeCacheMatrix <- function(x = matrix()) {
     creatematrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
     }
}


## cacheSolve - Test if cached, otherwise solve and return

cacheSolve <- function(x, ...) {
     cachesolve <- function(x, ...) {
          m <- x$getsolve()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
     }
}
 
