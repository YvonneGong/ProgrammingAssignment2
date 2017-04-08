## Put comments here that give an overall description of what your
## functions do

## This function is to create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- MULL ##initialize m as NULL
        set <- function(y) { ##define the set function
                x <<- y
                m <<- NULL
    }
        get <- function() x ## define get function to return matric arguement 
        setInverse <- function(inverse)m <<- inverse
        getInverse <- function()m
        list(set = set, get = get, ##return a list of functions 
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function compute the inverse of the matrtix returned by "makeCacheMatix"
## If the inverse matrix exists, the cacheSolve will retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) { ##check whether the inverse matrix exists
                message("getting cached data")
                return (m) ## if so, retrieve 
    }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
