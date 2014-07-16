## makeCacheMatrix() puts a given matrix into a container which can contain its inverse;
## cacheSolve() utilizes this "cache" to get a given matrix's inverse faster, if possible.

## Returns a list containing get/set operations, similar to insturctor's example. 
## The variable "container" serves as cache here while "x" is a matrix.

makeCacheMatrix <- function(x = matrix()) {
    container <- NULL
    set <- function(to_be_set) {
        x <<- y
        container <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) container <<- mat
    getinverse <- function() container
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns a matrix which is the argument's inverse. Attempt to "load" it from the "cache" first.
## If not exist, then solve it and return.
cacheSolve <- function(x, ...) {
    result <- x$getinverse()
    ## If cached
    if(!is.null(result) {
        message("Result cached, getting it")
        return(result)
    }
    ## If not cached
    mat <- x$get()
    result <- solve(mat)
    x$setinverse(result)
    result
}
