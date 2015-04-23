## Programming assignment 2.  Cacheing a matrix inversion  to avoid uneccessary
## recalculations on the same input matrix.

## This function creates a list that stores the following:
## set is a function that can reassign the value of the input matrix
## get is a function that retrieves the input matrix
## setinverse is a function that accepts the matrix inversion from cacheSolve
## getinverse is a function that passes the inversion received by set, 
##      if previously calculated

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## function is designed to calculate and store inverted matrix on first pass, 
##      but if rerun, recycles previous output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i

}
