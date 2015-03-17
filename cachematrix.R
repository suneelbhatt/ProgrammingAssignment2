## Put comments here that give an overall description of what your
## functions do




## A special vector is created which is a list of functions 
## to set the value of the vector, get the value of the vector
## to set the value of the mean and to get the value of the mean






makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}






cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



## function cacheSolve finds the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
