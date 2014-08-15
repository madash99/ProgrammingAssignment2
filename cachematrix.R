## This code creates a special matrix and caches the inverse
## Objective: If matrix doesn't change, and inverse exists in cache, retrieve it

# Function 1: makeCacheMatrix 
# Argument required is a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function 2: cacheSolve
## If inverse exists in cache and matrix has not changed, use it
## Otherwise, calculate for the first time drawing from first function def.'s

cacheSolve <- function(x, ...) {
       
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}        
