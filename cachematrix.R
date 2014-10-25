## makeCacheMatrix: creates a "matrix" object that can cache its inverse
##                  It's a list containing a function to
##                  set the value of the matrix (set)
##                  get the value of the matrix (get)
##                  set the value of the inverse of the matrix (setinverse)
##                  get the value of the inverse of the matrix (getinverse)
makeCacheMatrix <- function(x = matrix())
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix.
##             If the inverse has already been calculated (and the matrix has not changed), then cacheSolve prints message "getting cached inverse matrix" and retrieve the inverse from the cache

cacheSolve <- function(x, ...)
{
	inv <- x$getinverse()
	if(!is.null(inv))
	{
		message("getting cached inverse matrix")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}