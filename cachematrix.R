##  These two functions are designed to cache the inverse of a matrix to save recomputing repeatedly.


##  The first function creates a special "matrix" object that can cache its inverse.
##  This function uses the <<- operator to assign a value to an object in a different environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)            
}

##  The second function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated (and the matrix has not changed), then 
##  cacheSolve will retrieve the inverse from the cache rather than recomputing.

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
