## The functions create a special object that contain a matrix and its
## inverse matrix. Since calculation of the inverse matrix is usually a
## costly computation the result of the once calculated inverse matrix
## is cached

## The function makeCacheMatrix returns from a given matrix x a special 
## "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## set the matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the matrix value
    get <- function() x
    ## set the value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    ## get the value of the inverse matrix
    getinverse <- function() inv
    
    ## return the list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve returns the inverse matrix for the matrix that 
## contains in special "matrix" object x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## If the inverse of 'x' is cached returns the cached data
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse of 'x' is not cached calculates the inverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
