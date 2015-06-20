## This function will create a list with 4
## elements, all of which will be functions:
## get()
## set()
## getinverse()
## setinverse()
##
makeCacheMatrix <- function(x = matrix()) {
    ## i will hold the cached inverse of the matrix
    i <- NULL
    
    # set will set the matrix to a new matrix
    set = function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    ## get will return the matrix
    get = function() x
    
    ## setinverse will set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse will get the value of the inverse
    getinverse <- function() i
    
    ## and finally, we return a list with all of these functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that will take a cached 
## matrix created using the previous function and 
## calculate the inverse of it. 
##
## However, if it has an inverse cached, it will just
## return that instead.
##
cacheSolve <- function(x, ...) {
    ## get x's inverse and see if it has been cached
    i <- x$getinverse()
    if (!is.null(i)) {
        
        ## create a loading message
        message("getting cached data")
        
        ## return the cached inverse and exit the function
        return(i)
    }
    
    ## otherwise, calculate the inverse
    ## get x's matrix
    data <- x$get()
    
    ## calculate the inverse
    i <- solve(data)
    
    ## set the inverse
    x$setinverse(i)
    
    ## return i
    i
}