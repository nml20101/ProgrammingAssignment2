## Functions makeChaceMatrix and cacheSolve cache the inverse 
## of the matrix 

## makeCacheMatrix creates a 'vector' which is a list that
## contains a function to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse,
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # need to check if matrix is square
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function calculate the inverse of the 'vector'
## created via makeCacheMatrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
