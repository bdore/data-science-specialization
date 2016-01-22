##All comments written by Bernardo Doré

## The code below creates two functions. The first calculates the inverse of a given matrix
## and stores it in a cache. The second decides to set a new value to the cache if
## the matrix is changed or display the data in the cache if the matrix has not changed.

## Use this sample matrix to test if you like. You may test with a different matrix
## as long as it is an invertible matrix. To check if the matrix 'm' is
## invertible run 'det(m)'. If it returns a value different than zero it is invertible.
## A non-square matrix is not invertible.
## m <- matrix(c(-1, -2, 1, 1), 2,2)

##run the below code one at a time in console to test
##x <- makeCacheMatrix(m)
##x$get()
##cacheSolve(x)
##cacheSolve(x)

## function that calculates the inverse of the above matrix 'm 'and creates
## a cache for it. The cache is read by 'getInverse'. 'setInverse' stores the inverse
## in the cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function that checks if the cache is populated or not.
## If the cache is populated it displays the message and returns the value stored.
## If the cache is not populated it calculates the inverse of the matrix and stores it using
## 'setInverse' defined in 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        ## Tries to get the inverse of matrix 'm' from the cache
        m <- x$getInverse()
        
        ## If the cache is populated retrieves 'm' from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
