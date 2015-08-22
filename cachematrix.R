## makeCacheMatrix() returns a list containing 4 functions :

## set()        : caches (sets) a matrix
## get()        : gets the cached matrix
## setinverse() : caches (sets) the inverse of a matrix
## getinverse() : gets the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <-function() x
    setinverse <-function(solve) m <<- solve
    getinverse <-function() m
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse
    )
}

## cacheSolve() calculates the inverse of a matrix.

## It first checks to see if the inverse has already been cached.
## If the cached matrix exists, cacheSolve() returns the cached inverse.
## Otherwise, it calls solve() to calculate the inverset and returns the result

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if ( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}
