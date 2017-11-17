## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The inverse of the matrix "x" is "inv.x"

makeCacheMatrix <- function(x = matrix()) {
	inv.x <- NULL
	set <- function(y) {
		x <<- y
		inv.x <<- NULL
		}
	get <- function() x			#return matrix
	setinverse <- function(solve)		#save the solve value
		inv.x <<- solve		
	getinverse <- function() inv.x	#retrieve solve value
	list (					#return list
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
		
}


## The CacheSolve function returns the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been created, CacheSolve gets the inverse from the cache.
## If not, CacheSolve calculates the inverse.

cacheSolve <- function(x, ...) {
	
	inv.x <- x$getinverse()
	if (!is.null(inv.x)) {
		message ("getting cached data")
		return (inv.x)
	}
	xdata <- x$get()			#get data from the matrix
	inv.x <- solve (xdata,...)		#use solve to inverse
	x$setinverse (inv.x)		#save solve value
	inv.x

}


