## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#In general, this function "makeCacheMatrix" creates a matrix with some criteria showed below, and "cacheSolve" calls its inverse:

makeCacheMatrix <- function(x = matrix()){
    m <- NULL                             #Sets the "NULL" value as default to "m" 
    set <- function(y){                     #Creates a new function "set"                                         
        x <<- y                             #with a new value set to x in the parent environment
        m <<- NULL                        #it founds a new matrix, returns NULL
    }
    get <- function() {x}                   #creates a new function called "get" which returns the matrix (x)
    setInverse <- function(inverse) {m <<- inverse}                               #assign the value "inf" inside the parent environment
    getInverse <- function() {m}                                                  #gets "m" in the environment where it's called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    #these equal signs are needed to use "$" in the next step as a part of a whole.
}


## Gives a matrix which is the inverse of the given matrix created above. 
## If the inverse was already calculated, then the function returns the inverse of that one in cache

cacheSolve <- function(x, ...){
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$setInverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
