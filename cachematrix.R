# The following two functions are used collectively to calculate and cache matrix inversion
# if not already cached, or retrieved cached matrix inversion if already cached.

# Caches a matrix (object "mtrx") and its inverted counterpart (object "inv")
# with functions that can set or retrieve both.

makeCacheMatrix <- function(mtrx = matrix()) {
     inv <- NULL
     setmtrx <- function(x) {
          mtrx <<- x
          inv <<- NULL
     }
     getmtrx <- function() mtrx
     setinv <- function(x) inv <<- x
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


# The below function tries to retrieve cached inverted matrix
# and calculates and caches inversion if no cached data exist.

cacheSolve <- function(cachebox) {
     inv <- cachebox$getinv()
     if(!is.null(inv)) {
          message("Getting cached inverted matrix...")
          return(inv)
     }
     mtrx <- cachebox$getmtrx()
     inv <- solve(mtrx)
     x$setinv(inv)
     inv
}