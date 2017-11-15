## makeCacheMatrix and cacheSolve are a pair of functions which attempt to calculate
## the inverse of a matrix (using the solve() function) and store it in the memory 
## cache, enabling it to be recalled easily in further calcullations. One must 
## provide store the result of makeCacheMatrix in a new value for it to work.

## makeCacheMatrix creates 2 objects and 4 functions, the first object is an argument
## which stores input data, the second stores data processed by cacheSolve. 
## The 4 functions are actually two pairs of accesing and 
## defining functions, one pair for raw data the other for processed data (in this case
## a matrix and the inverse, respectively). Also, all objects are defined in the same
## environment, using "<<-" and a naming list for "set", "get", "setsolve" and "getsolve".

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve takes advantage from lexical scoping and retrieves the value of the object
## stored in "s". If there is a non NULL value stored in "s", it returns the message
## "getting cached data" and the value, whereas if it is NULL it, returns "NULL".
## The other thing this function does is calculate the inverse of the matrix stored
## in makeCacheMatrix, and store it in "s".

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}