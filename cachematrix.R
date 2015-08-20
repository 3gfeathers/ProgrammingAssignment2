# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# 
# Study in lexical scoping

# makeCacheMatrix creates a matrix and a list that stores 4 functions that:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of the inverse of the matrix
# 4. gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # inv is the vector holding the inverse of the matrix and is instantiated 
    # to the NULL value
    inv <- NULL
    
    # function that changes the vector that is stored when makeCacheMatrix
    # is called
    set <- function(y) {
        
        # replace vector x with argument y in the parent function makeCacheMatrix
        # as opposed to just within the scope of the set function
        x <<- y
        
        # resets inv to the NULL value in the parent function makeCacheMatrix
        # as opposed to just within the scope of the set function. inv needs to 
        # be recalculated through the cacheSolve function
        inv <<- NULL
    }
    
    # function that retrieves the vector x that is created and stored when
    # makeCacheMatrix is called
    get <- function() x
    
    # function that changes the vector inv in the parent function
    # makeCacheMatrix to the argument vector "inverse"
    # note: does not actually calculate the inverse but merely sets the value
    # to the value of the given argument
    setinverse <- function(inverse) inv <<- inverse
    
    # function that retrieves the vector holding the inverse matrix
    getinverse <- function() inv
    
    # stores the set, get, setinverse and getinverse functions in makeCacheMatrix
    # using the list() function so that an object created through makeCacheMatrix
    # will have the functions available
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    
    # call the getinverse function (as a name from the list of functions)
    inv <- x$getinverse()
    
    # verifies the value if inv (stored with getinverse) exists and is not NULL
    if(!is.null(inv)) {
        
        # if inv exists and is not NULL, return a message and the cached value
        # of inv...end of function
        message("getting cached data.")
        return(inv)
    }
    
    # if inv does not exist or is NULL (alternate "else" code)
    
    # data receives the vector stored with makeCacheMatrix
    data <- x$get()
    
    # inv stores the inverse of the matrix "data" as calculated with the solve
    # function
    inv <- solve(data)
    
    # x stores the inverse matrix in the makeCacheMatrix object
    x$setinverse(inv)
    
    # return the inverse matrix stored in inv
    inv
}