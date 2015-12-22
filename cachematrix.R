## Created a Class makeCacheMatrix which takes a matrix and allows the user to
## create an Object that will store it's inverse

## makeCacheMAtrix class contains a matrix and also allows for storage of the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(
	 set=set, 
	 get=get, 
	 setinverse=setinverse, 
	 getinverse=getinverse
	)

}


## cacheSolve determines if the makeCacheMatrix object received has a inverse
## matrix stored.  If it doesn't have it stored it will compute the inverse 
## and return it.  If it does have it stored it will return inverse as found in the
## inv field.


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
