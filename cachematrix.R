## The following functions create a special matrix object that can cache its inverse 
## and then computes the inverse. If the inverse has already been calculated and matrix
## is unchanged, then the cacheSolve function retrieves the inverse from the cache instead
## of recalculating it.

## makeCacheMatrix creates a special "matrix" object which contains function to
## set the value of the matrix object
## get the value of the matrix object
## set the value of the inverse object
## get the value if the inverse object

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
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


## The following function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function above. If the inverse has already been calculated and the matrix
## is unchanged, then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## checks if the inverse has already been computed and if has, throws the message
        ## "getting cached data"
        if (!is.null(m)) {
                 message("getting cached data")
                 return(m)
        }
        ## else if it is not in the cache yet, it computes it here
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
