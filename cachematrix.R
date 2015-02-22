## The two functions below work together to cache the inverse of a matrix and retrieve it when needed later. 

## Limitation 1: Assumed the input matrix is invertible. 
## Limitation 2: The function does not evalue if the actual value of the given matrix or calculated inverse of the matrix is the same to the previous ones. 
## To take advantage of reading computed results from the cache, 
## the output of makeCacheMatrix() needs to be captured and re-entered in cacheSolve with the same variable (cache) name everytime. 

makeCacheMatrix <- function(x=matrix()) {
        ## This function creats a special matrix object that can cache its inverse.
        
        ## x is a matrix and assumed to be invertible under the context of the course assignment.
        
        ## Returns a list allowing retrieval of cached computed results if exists 
        
        ## initialize the temporary variable m
        m <- NULL
        
        ## Assign values to variables in cache, i.e. to be stored outside the function
        set <- function(y) {
                x <<- y     ## define the matrix in cache 
                m <<- NULL  ## clears m when the input matrix has been changed.
        }
        
        ## function allow retrieving values from cache 
        get <- function() x
        
        ## Assign the value of the inverse matrix to the variable in cache
        setInv <- function(solve) m <<- solve
        
        ## Retrieve the inverse matrix stored in cache
        getInv <- function() m
        
        ## Assemble the list to return
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv
             )
}


cacheSolve <- function(x, ...) {
	## This function is to computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
	## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        
	## x is the list returned from makeCacheMatrix.
        
	## Returns the inverse matrix cached or newly computed if the matrix has been changed.
        
        ## assign the value of inverse matrix from cache to the predefined variable
        m <- x$getInv()
        
        ## if the value exists in the cache, return the retrieved value and report the fact that the data is from cache.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the value does not exist in the cache, compute the inverse matrix and assign it to variable.
        data <- x$get()       ## retrieve the value of the giving matrix
        m <- solve(data, ...) ## assign inverse matrix to local variable m
        x$setInv(m)           ## assign the inverse matrix to the variable in cache
        m                     ## return the matrix that is the inverse of x
}