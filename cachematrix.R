## These functions will create the inverse of a matrix and store that inverse as
## a cached value. cacheSolve requires the list output of makeCacheMatrix as its
## input.

## With a matrix input, returns a series of functions that will do the following:
## set will set the value of a matrix
## Get will return the value of the matrix
## Setinverse will set the value of the matrix inverse
## Getinverse will return the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## First checks to see if there is already a cached inverse matrix. If none,
## calls the original matrix and then solves for the inverse. Finally, caches 
## and then returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("Retrieving cached value")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
