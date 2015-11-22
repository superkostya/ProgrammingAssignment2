## Function makeCacheMatrix: creates a special "matrix" (object?), 
## which contains a list of functions that do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

        mInv <- matrix()
        set <- function(y) {
                x <<- y
                mInv <<- matrix()
        }
        get <- function() x
        setInv <- function(mInverted) mInv <<- mInverted
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Function cacheSolve: Calculates the inverse of the special "matrix" 
## created with the above function makeCacheMatrix. 
## However, it first checks to see 
## if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
