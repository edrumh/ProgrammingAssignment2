## Put comments here that give an overall description of what your
## functions do

## Okay I parralleled the example here, but changed mean to inverse
## m <- NULL will initialize the memory to empty
## set <- function(y)  allows you to change the value of global x and reinitialize m
## get returns the value global x
## setinverse sets the value of global m
## getinverse returns the value of global m if exists

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function
## first check to see if x has getinverse aka m defined
## if it does just return m (which is the solve of data X)
## if it doesn't 
##          then get x value back into data
##          then set the m value to the solve() of the data
##          store the inverse results into memory 
##          return the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
