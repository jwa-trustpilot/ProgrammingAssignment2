## R Programmin assignment 2 - w3
## These functions are able to cache the inverse of an inversable matrix, making it faster to
## retreive the inverse of a matrix if it has already been calculated
## 
## I have based my solution on the example provided in the task description and adjusted to fit
## the solution to the task

## To be able to cache the inverse of a matrix we first need a function that creates a special
## matrix object that includes the following functions:
## 
## 1. Function to set the value of the matrix
## 2. Function to get the value of the matrix
## 3. Function to set the value of the inverse matrix
## 4. Function to get the value of the inverse matrix
## 
## This funtion will be used by the next function to cache the inverse matrix calculated by this 
## function. For this we among other things use the ability to assign values to objects outside of
## the current environment (<<-)

makeCacheMatrix <- function(x = matrix()) {
        # We define the inverse matrix object to be able to use it later
        inv <- NULL
        
        #This is the funtion that sets the actual matrix that we calculate the inverse of
        #This will be used if we are calling the function for a new matrix 
        set <- function(y){
                #Assigning the value of the matrix to the matrix-object x
                x <<- y
                
                #Resets the inverse matrix object if it has previously been set
                inv <<- NULL
        }
        
        #Function that returns the matrix provided as argument in this makeCacheMatrix-function
        get <- function() x
        
        #Function that calculates the inverse of the matrix provided
        #This is where we store the inverse matrix to the inverse matrix object defined above
        setinv <- function(i) inv <<- i
        
        #Function that returns the inverse matrix calculated
        getinv <- function() inv
        
        #Defines a list of the defined funtions so that all types of this object has access to 
        #these functions. We also apply the items names so that they are easier to extract
        list <- list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function will calculate the inverse matrix and set the value in the makeCacheMatrix function
## so that it will not need to be calculated again if the function is called on a already called 
## matrix object. The argument x needs to be of the makeCacheMatrix type

cacheSolve <- function(x, ...) {
        ## First we will check if there is already an inverse matrix set for the argument provided
        ## We will use getinv to extract the inverse matrix from the makeCacheMatrix-object
        inv <- x$getinv()
        
        ## We check if the inv object is empty or not. If the object contains the inverse matrix
        ## it will be returned with a message about retrieving the cached inverse matrix
        if(!is.null(inv)){
                message("Getting the cached inverse matrix")
                return(inv)
        }
        
        ## If the inv object does not contain an inverse matrix we need to set it. For this we use
        ## the setinv function. First we need to get the matrix object
        matrix <- x$get()
        
        ## Then we calculate the inverse matrix of the matrix-object using the solve-funtion
        inv <- solve(matrix)
        
        ## Finally we set the inverse matrix and return it
        x$setinv(inv)
        return(inv)
}
