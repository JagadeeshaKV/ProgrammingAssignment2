## Put comments here that give an overall description of what your
## functions do

##--------------------------------------------------------------------------##
## The function 'makeCacheMatrix' creates a special matrix which is 
## really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## The second function 'cacheSolve' calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. However, it first 
## checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the solve function
##--------------------------------------------------------------------------##

## Write a short comment describing this function
##--------------------------------------------------------------------------##
## This function creates a special "matrix" object that can cache its inverse.
##--------------------------------------------------------------------------##
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(input){
        x <<- input
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(sMatrix) inverse <<- sMatrix
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function
##--------------------------------------------------------------------------##
## The function 'cacheSolve'computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.
##--------------------------------------------------------------------------##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse  
}
