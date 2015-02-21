## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

makeCacheMatrix <- function(x=matrix()) {
        ## This function creats a special matrix object that can cache its inverse.
        
        ## x is a matrix and assumed to be invertible under the context of the course assignment.
        
        ## Returns a list of functions 
        
        ## initialize the temporary variable m
        m <- NULL
        
        ## 
        set <- function(y) {
                x <<- y     ## define the matrix in cache 
                m <<- NULL  ## clears m when the input matrix has been changed.
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv
             )
}

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}