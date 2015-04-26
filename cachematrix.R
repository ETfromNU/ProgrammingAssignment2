## The functions defined here are able to
## both calculate and cache the inverse of a matrix.
## We create the cache to avoid the potentially costly 
## computation of recalculating the inverse of a matrix if
## it has already been calculated during a session.



## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set a cached value representing the inverse of the matrix
## 4. get a cached value representing the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initializing the cached version of the inverse matrix to NULL  
    cachedInvMatrix <- NULL
    
    ## Set the value of the "original" matrix and clear the cache
    ## (i.e. - set the value of the cached matrix to NULL)
    set <- function(y) {
        x <<- y
        cachedInvMatrix <<- NULL
    }
    
    ## Retrieve the value of the "original" matrix
    get <- function() x
    
    ## Set the value of the cached inverse matrix
    setInvMatrix <- function(invMatrix) cachedInvMatrix <<- invMatrix
    
    ## Retrieve the cached inverse matrix
    getInvMatrix <- function() cachedInvMatrix
    
    ## Return a list of the functions defined in makeCacheMatrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Retrieve the cached inverse matrix
    inverse <- x$getInvMatrix()
    
    ## Check the value of the cached inverse matrix.  If it is not null,
    ## then return the cached version of the inverse matrix.
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    ## Get the non-inverted matrix
    origMatrix <- x$get()
    
    ## Calculate the inverse of the matrix 
    ## note: this step can be computationally intensive depending on
    ## the size of the matrix
    inverse <- solve(origMatrix)
    
    ## Cache the value of the inverted matrix
    x$setInvMatrix(inverse)
    
    ## Return the inverted matrix
    inverse
}
