## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
    i <- x$getinverse()
    d <- dim(x$displaymat())
    if(!is.null(i))
    {
        message("Cached inverse matrix:")
        return(i)
    }
    else if (!det(x$displaymat()) && !(d[1] - d[2]))
    {
        message("Cannot calculate the inverse global condition failure!!!")
        return(NULL)
    }
    data <- x$displaymat()
    i <- solve(data)

    message("Matirx inverse was found - Stored to cache")
    x$storeinv(i)
    x$getinverse()
}
