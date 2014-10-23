## This R file contains two functions. 1)makeCacheMatrix 2)cacheSolve
# Both the functions together provides the ability to cache a matrix once, set its inverse
# Then read the matrix and its inverse from cache as required

# makeCacehMatrix accepts a matrix input and set the four functions to the object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setInv <- function(xInv) inv <<- xInv
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# cacheSolve is used to get a matrix inverse and 
# if cache has the inverse already, then its taken

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data ...")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInv(inv)
    inv
}
