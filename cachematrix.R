## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#In general, this function "makeCacheMatrix" creates a matrix with some criteria showed below, and "cacheSolve" calls its inverse:

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL                             #Sets the "NULL" value as default to "inv" 
    set <- function(y){                     #Creates a new function "set"                                         
        x <<- y                             #with a new value set to x in the parent environment
        inv <<- NULL                        #it founds a new matrix, returns NULL
    }
    get <- function() {x}                   #creates a new function called "get" which returns the matrix (x)
    setInverse <- function(inverse) {inv <<- inverse}                               #assign the value "inf" inside the parent environment
    getInverse <- function() {inv}                                                  #gets "inv" in the environment where it's called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    #these equal signs are needed to use "$" in the next step as a part of a whole.
}


## Gives a matrix which is the inverse of the given matrix created above. 
## If the inverse was already calculated, then the function returns the inverse of that one in cache

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
