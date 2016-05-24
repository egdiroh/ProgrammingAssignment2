## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # define the inverse, it hasn't been computed yet
    i <- NULL
    #define the set function
    set <- function(y) {
        x <<- y
        #for a new value, we haven't computed the inverse yet
        i <<- NULL
    }
    #define the get function
    get <- function() x
    #define the setinverse function
    setinverse <- function(inverse) i <<- inverse
    # define the get inverse function
    getinverse <- function() i
    #return a list of the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # check the inverse in the cache
    i <- x$getinverse()
    # if it's been defined go ahead and return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # otherwise we need to get the matrix to sole it
    data <- x$get()
    # now we solve for the inverse
    i <- solve(data, ...)
    # now we set it in the cache
    x$setinverse(i)
    #finally the inverse will be our return value
    i
}
