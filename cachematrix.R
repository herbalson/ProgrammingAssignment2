## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a vector which is a matrix containing a function to
##set the value of the vector
##get the value of the vector
##set the values of the matrix
##get the values of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of a matrix using the solve function.
##if already calculated it gets the inverse, otherwise it calculates the inverse and sets it

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
