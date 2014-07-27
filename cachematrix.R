## Below you can find two functions 'makeCacheMatrix' and 'cacheSolve'. 
## The first function creates the special object that can cache it's inverse. 
## The second function returns the inverse of the matrix created by the first function.

## The function 'makeCacheMatrix' creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function 'cacheSolve' takes special object created by the previous function and returns the inverse of it.

cacheSolve <- function(x, ...) {
       
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
