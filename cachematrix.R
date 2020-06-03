## Here we have two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function below creates a list that containing the matrix, inverse of
## matrix and the functions "get's" and "set's" to the matrix and his inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        setMatrix <- function(y) {   
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x    
        setinv <- function(inverse)inv <<- inverse  
        getinv <- function() inv    
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinv = setinv,
             getinv = getinv)
}


## The function below verify if the inverse has been calculated, if the 
## condition is true, it just return the inverse of matrix, otherwise, 
## the function calculate and set the inverse of matrix

cacheSolve <- function(x, ...) {
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting inversed matrix")
                return(I)
        }else{
		mat <- x$getMatrix()
        	I <- solve(mat, ...)
        	x$setinv(I)
        	I
	  }      
}