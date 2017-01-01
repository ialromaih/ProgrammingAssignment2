## Cache Solve returns the inverse of a matrix (X). The function is optimized to cache the results
## and reuse it for subsequent calls.

## MakeCacheMatrix creates and initializes the local variables and include all setters/getters

makeCacheMatrix <- function(x = matrix()) {
     x_inv <- NULL
     set <- function(y){
        x <<- y
        x_inv <<- NULL
     }
     get <- function() x
     setinv <- function(inv) x_inv <<- inv
     getinv <- function() x_inv
     list(set=set, get=get,
          setinv = setinv,
          getinv = getinv)
}


## Returns the inverse of the matrix (X)
## The function fetches the cache first to reuse results from previous runs, otherwise, the inverse is evaluated.

cacheSolve <- function(x, ...) {
        #x_df <- as.data.frame(x)
        x_inv <- x$getinv()
        if(!is.null(x_inv)){
                return(x_inv)       
        }
        data <- x$get()
        x_inv <- solve(data,...)
        x$setinv(x_inv)
        x_inv
        ## Return a matrix that is the inverse of 'x'
}
