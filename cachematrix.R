## These functions can be used in calculating the inverse of a matrix in order to reduce the time required to repeatedly compute.

## The makeCacheMatrix funtion creates a list to set the value of the matrix, retreive the value of the matrix, as well as setting and retreiving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the matrix created by the makeCacheMatrix.  This function checks to see if the inverse of the matrix has already been calculated.  If it has, it gets that inverse from the cache and avoids the computation.  If not, it calcluates the inverse of the data and then sets the inverse matrix in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	message("Getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
