## makeCacheMatrix stores the orginal matrix to be inverted as well as the inverted matrix if it has been already inverted by the cacheSolve function. For new matrices, makeCacheMatrix resets the values and variables so the list can be feeded again with new data, storing new matrices. 
## cacheSolve for a new matrix, calculates the inverse of the matrix, returning and storing it in makeCacheMatrix. When the inverse matrix want to be printed, there is no need to recalculating it because cacheSolve calls makeCacheMatrix function and pulls from it the stored inverted matrix, skiping -therefore- additional calculations.

## makeCacheMatrix retruns a list with: i) a cached inverse matrix (if any), ii) stored new inverse matrix, iii) matrix (argument) to be inverted, and iv) (re)sets inverse matrix for new matrices that want to be entered.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns the inverse matrix pulling it out from the cache or calculating it the very first time. When calculates the inverse matrix for the first time, it stores the inverse in the list that serves as argument for this function so no future calculations will be done.

cacheSolve <- function(x, ...) {
       i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}