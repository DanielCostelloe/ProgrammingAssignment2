## Put comments here that give an overall description of what your
## functions do


## Basing this function on the makeVector funciton that was given 
##    to us as part of the assignment instructions
## As per the assignment instructions:
##   "This function creates a special 'matrix' object that can cache its inverse."

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## changed mean to solve to get inverse matrix
    setmean <- function(solve) m <<- solve
    getmean <- function() m
    ## return a list
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## Basing this function on the cachmean funciton that was given 
##    to us as part of the assignment instructions
## As per the assigment instructions:
##    "This function computes the inverse of the special
##    'matrix' returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    'cacheSolve' should retrieve the inverse from the cache."
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## changed mean to solve to get inverse matrix
    m <- solve(data, ...)
    x$setmean(m)
    m
}

## Example usage:
## testm<-matrix(c(1,5,11,1),2,2)
## testm
## mm_returned <- makeCacheMatrix(testm)
## mm_returned

## cs_returned <- cacheSolve(mm_returned)
## cs_returned

