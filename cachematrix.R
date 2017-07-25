## Caching the inverse of a matrix

## Creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(inver) i <<- inver
        getinver <- function() i
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)

}


## Computes the inverse of the special "matrix" returned
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinver()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinver(i)
        i
}
