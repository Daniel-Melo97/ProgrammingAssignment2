## Here we have two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function below creates a list that containing the matrix, inverse of
## matrix and the functions "get's" and "set's" to the matrix and his inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
	  
	  #this function will create the matrix	
        setMatrix <- function(y) {   
                x <<- y
                inv <<- NULL
        }
	  
	  #this function will return the matrix
        getMatrix <- function() x
	  
	  #this function will set the inverse    
        setinv <- function(inverse)inv <<- inverse 
	  
	  #this function will return the inverse	 
        getinv <- function() inv

	  #returning the list with the 'special' matrix	    
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinv = setinv,
             getinv = getinv)
}


## The function below verify if the inverse has been calculated, if the 
## condition is true, it just return the inverse of matrix, otherwise, 
## the function calculate and set the inverse of matrix

cacheSolve <- function(x, ...) {
	  # I will recieve the inverse	
        I <- x$getinv()
	
	  #if the inverse isn't null, it will just return the inverse
        if(!is.null(I)) {
               
		    message("getting inversed matrix")
                return(I) #return the inverse of matrix

        }else{#if I is null, Mat will recieve the matrix
		mat <- x$getMatrix()
        	I <- solve(mat, ...) #I will recieve the inverse of matrix
        	x$setinv(I) # the object will set the inv to recieve the value of I
        	I #return the inverse of matrix
	  }      
}