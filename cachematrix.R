## The pair of functions below (that is makeCacheMatrix and cacheSolve) together cache the 
#inverse of a matrix




#The makeCacheMatrix functions create a special matrix 
#that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix  <- function() m 
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setmatrix(m)
    m
}
