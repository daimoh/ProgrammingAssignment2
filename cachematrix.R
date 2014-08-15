
# makeCacheMatrix is the "constructor" of the matrix object
# Usage:
#    new_matrix_object <- makeCacheMatrix(x)
## where x can be a pre-existing matrix, created with something like:
##   x <- structure(c(1, 2, 2, 2), .Dim = c(2L, 2L))
#      [,1] [,2]
# [1,]    1    2
# [2,]    2    2
# The following methods are available for the new_matrix_object:
#       set(y)                      which sets the matrix to be equal to y
#       get()                       which returns the matrix
#       setinverse(new_inverse)     which sets the inverse of the matrix
#       getinverse()                which returns the inverse of the matrix
#                                   Note this will return a cached version of
#                                   the matrix if it has already been
#                                   calculated and the matrix has not changed
makeCacheMatrix <- function(x = matrix()) {
    # we're using inverse_x as the cached copy of the inverse of x,
    # and as we're creating it for the first time, we don't know what the
    # inverse is, so we set it to NULL (nothing)
    inverse_x <- NULL

    # the set method for the matrix, which sets the matrix x to the
    # matrix y that is passed in.
    set <- function(y) {
        # search through parent environments for an existing definition
        # of x that is being assigned. If x is found (and its
        # binding is not locked) then its value is redefined, otherwise
        # assignment takes place in the global environment. Do the same
        # for inverse_x. Essentially, set the value of the matrix to y,
        # and clear the cached version of the inverse.
        x <<- y
        inverse_x <<- NULL
    }
    # the get method for the matrix, returns the matrix value
    get <- function() x
    
    # set the inverse of the matrix to the value passed in (new_inverse)
    setinverse <- function(new_inverse) inverse_x <<- new_inverse
    
    # return the value of the inverse
    getinverse <- function() inverse_x
    
    # the list of available methods and their corresponding functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## check the matrix object cache for the index,
## if it does exist in the cache, then return the cache
## if it doesn't exist, then solve the inverse and cache it
cacheSolve <- function(x) {
    # Return a matrix that is the inverse of 'x'
    # Get the inverse matrix from the matrix object
    inverse_x <- x$getinverse()

    # if the inverse isn't null, then the cache has the inverse
    # so we can return that
    if(!is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
    }
    # if inverse is null, it hasn't been created,
    # so create it
    data <- x$get()
    inverse_x <- solve(data)
    # and now that we've created it, we should set it
    x$setinverse(inverse_x)
    # and return it
    inverse_x
}
