# This function creates an object that can cache an inverse of a matrix.
# The crux is the "<<-"" statement in the lines 7,8 and 11. Here objects will be stored in  
# an environment above the level in which it was defined.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# This function returns the inverse of a matrix. The if statement checks if 
# the inverse has been stored. If the object "inv" has a value, this value will be printed. 
# Otherwise the inverse will be computet with the "solve" statement and stored in the cache with
# the "setinverse" statement.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
