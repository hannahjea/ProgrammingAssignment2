## Functions to allow for caching of a martix's inverse - since calculating the inverse can be a costly operation.

## This function creates a special "matrix" object that can cache its inverse.
## It holds the matrix, its inverse, and all the getter and setter functions for these variables.

makeCacheMatrix <- function(x = matrix()) {
    ## initialise variable for the inverse, inv
    inv <- NULL
    
    ## define setter for x (set x and null the inverse)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## define getter for x
    get <- function() x
    
    ## define setter for inv of x
    setinverse <- function(inverse) inv <<- inverse
    
    ## define getter for inv of x
    getinverse <- function() inv
    
    ## retuen the list of functions for later access
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Takes a makeCacheMatrix object as input.

cacheSolve <- function(x, ...) {
    ## get cached inverse of x
    inv <- x$getinverse()
    
    ## check if cached inverse is null, if not null then return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if cached inverse is null then get x, calculate the inverse, then cache the inverse back to makeCacheMatrix object
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    ## return calculated inverse
    inv
}
