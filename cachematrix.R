## makeCacheMatrix() is a function which constructs a new matrix object from x (an R matrix)
## and returns a list of functions (set(), get(), setinv(), getinv()).
##
## cacheSolve() is a function which, from x (a new matrix object constructed by makeCacheMatrix()),
## returns an R matrix that is the inverse of x$get().

## The new matrix object returned by makeCacheMatrix() is basically a list of four functions : 
## set() is a function that sets the value of the object (x in this case),
## get() is a function that extracts the value from the object,
## setinv() is a function that stores the given matrix value as inverse for future use (NULL
## if it hasn't been computed yet) and getinv() is a function that extracts the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## If the inverse has already been calculated, cacheSolve() gets the inverse from the cache
## and skips the computation (it then throws a message), otherwise it simply computes the
## inverse of x and sets the value of the inverse in the cache via the setinv() function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
