## The following 2 functions cache the inverse matrix instead of
## repeatedly running time-consuming matrix inversion calculations.

## Following function creates a matrix object that caches its inverse
## to save time in calculation later.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    m <- NULL ## cache is initialized empty
    ## define the set function
    set <- function(y){
        x <<- y ## setting y to be the matrix
        m <<- NULL ## still NULL; inverse has not yet been calculated
    }
    get <- function() x ## return the cached matrix
    setInverse <- function(solve) m <<- solve(x) ## cache the inverse matrix
    getInverse <- function() m ## return the cached matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## pass along the functions
}

## Following function computes the inverse of a given matrix, if not already cached
## The matrix is returned by the above function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
    ## If the inverse has already been calculated (and the matrix has not changed), 
    ## then the cachesolve should retrieve the inverse from the cache.
    m <- x$getInverse() ## get the cached Inverse Matrix
    if(!is.null(m)) {
        message("getting cached data") ## m is not NULL; something is cached
        return(m) ## return cached inverse matrix
    }
    data <- x$get() ## nothing is cached, grab the matrix to be inverted
    m <- solve(data, ...) ## invert the matrix here
    x$setInverse(m) ## cache this new value
    m ## return the inverse
    
}