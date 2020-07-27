## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Author: Paul P Vinod (Git username: Varun1914)
## Task : Programming Assignment Week 3




##  Function to entervalue,display
## and storeinverse and reterive inverse value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    enterval <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    displaymat <- function()
    {
        x
    }
    storeinv <- function (y)
    {
        inverse <<- y
    }
    getinverse <- function ()
    {
        inverse
    }
    list(enterval = enterval, displaymat = displaymat,
         storeinv = storeinv, getinverse = getinverse)
}


## Function to calculate inverse of the created matrix
## else print the cached inverse matrix value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x
    i <- x$getinverse()
    d <- dim(x$displaymat())

    ## To check for the cached memmory
    if(!is.null(i))
    {
        message("Cached inverse matrix:")
        return(i)
    }

    ## To check for the global conditions necessary for inverse matrix
    else if (!round(det(x$displaymat())) && !(d[1] - d[2]))
    {
        message("Cannot calculate the inverse global condition failure!!!")
        return(NULL)
    }

    ## Inverse calculation
    data <- x$displaymat()
    i <- solve(data)

    message("Matirx inverse was found - Stored to cache")
    x$storeinv(i)
    x$getinverse()
}
