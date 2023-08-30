## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix  creates a special "matrix", which is really a matrix containing a function to set the value of the vector, 
# to get the value of the vector, to set the value of the mean, and to get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(solve(m))
        }
        data <- x$get()
        m <- solve(data, ...)
        x$solve(m)
        m
}
