## The module below caches the results of a matrix inverting

## This function creates an object that enables
## to store (cache) the result of a matrix inverting

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(y) i <<- y
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function uses the object created above
## to return a matrix inverted

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) return(m)
        m <- solve(x$get())
        x$setinverse(m)
        return(m)
}
