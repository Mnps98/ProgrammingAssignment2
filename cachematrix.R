#In this assignment we will assume that all the matrices are possible to invert
#there are two functions, makeCacheMatrix and cacheSolve

## Write a short comment describing this function
###This function creates a matrix and then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}    ## <- this function gets the matrix we want
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv} ## <- inverse the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#Used to get the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                print("getting cashed data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv      ## Return a matrix that is the inverse of 'x'
}
