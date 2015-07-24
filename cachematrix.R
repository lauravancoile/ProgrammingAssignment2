## Below you can find a pair of functions that cache the inverse of a matrix.


## The first function creates a special "matrix" object that can cache its inverse.
## This function is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get, setsolve=setsolve, getsolve = getsolve)
}

## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache and skip the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via the setsolve function.
cacheSolve <- function(x,...){
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
