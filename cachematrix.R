## Functions makeChaceMatrix and cacheSolve cache the inverse 
## of the matrix 

## makeCacheMatrix creates a 'vector' which is a list that
## contains a function to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse,
## and get the value of the inverse
## Example to test the functions:
## create matrix m1
## > m1 <- matrix(c(1,2,3,4), nrow =2, ncol =2)
## create special 'vector'
## > m1_spec = makeCacheMatrix(m1)
## attributes(m1_spec) 
## > $names
## > [1] "set"      "get"      "setsolve" "getsolve"
## execute caseSolve
## > cacheSolve(m1_spec)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## execute caseSolve again
## > cacheSolve(m1_spec)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

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


## cacheSolve function calculate the inverse of the 'vector'
## created via makeCacheMatrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # check if the matrix is square and its determinat is not zero
        dim1 <- dim(x$get())[1]
        dim2 <- dim(x$get())[2]
        if (( dim1!= dim2 ) | (det(data) != 0 )) {
                print("Matrix is not invertible")
                stop
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
