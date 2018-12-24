## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inveMatrix <- NULL

        setMatrix <- function(y) {
                x <<- y
                inveMatrix <<- NULL
        }
        
        getMatrix <- function() x                              
        setInverse <- function(inverse) 
        inveMatrix <<- inverse 
        getInverse <- function()
        inveMatrix                 
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

##function cacheSolve takes the output of the makeCacheMatrix as an 
##input and checks inverse matrix from it if has any value or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inveMatrix <- x$getInverse()
        if(!is.null(inveMatrix)) {                      
                message("Cached Invertible Matrix")  
                return(inveMatrix)                         
        }
        
        MatrixData <- x$getMatrix()                     
        inveMatrix <- solve(MatrixData, ...)            
        x$setInverse(inveMatrix)                         
        return(inveMatrix)
}
