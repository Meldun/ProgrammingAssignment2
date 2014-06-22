## This functions give the Inverse of a square matrix and store it on a cache
## If the Inverse has already been calculated, returns the cache result

## The makeCacheMatrix returns a list contaning 4 functions

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {  ## Set the value of vector
                x <<- y
                m <<- NULL
        }
        
        get <- function() x  ## Get the value of vector
        
        setsolve <- function(solve) m <<- solve ## Set the value of Inverse Matrix
        
        getsolve <- function() m ## Get the value of Inverse Matrix
        
        list(set = set, get = get ,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve verifies if the Inverse of Matrix was already computed
## and returns the cache result or solves the inverse of a new matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve() ## Getting the value of Inverse Matrix
        
        if(!is.null(m)) {  ## Verifying if result already in cache
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()           ## if not in cache it solves the Inverse
        m <- solve(data, ...)     ## and stores the result in cache
        x$setsolve(m)
        m
}
