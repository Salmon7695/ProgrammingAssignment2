## By using function makeCacheMatrix and cachesolve together
##This function could get a matrix and evaluate its inverse

## set matrix and let I be the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function()
        x
    setinverse <- function(solve)
        I <<- solve
    getinverse <- function()
        I
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## get matrix from function make Cachematrix and get its inverse

cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if (!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
    ## Return a matrix that is the inverse of 'x'
}
