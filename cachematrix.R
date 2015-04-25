## Name: makeCacheMatrix
## Parameters:matrix
## Takes a matrix as an argument and then stores it in it's cache, but 
##   also provides methods to store another value by exposing set/get on the 
##   original matrix, and also setinverse/getinverse on the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) i <<- inverse_matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Name: cacheSolve
## Parameters: special matrixB = 
## Takes a matrix and returns the inverse of that matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
