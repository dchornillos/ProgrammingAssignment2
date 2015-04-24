## The following two functions are used to:
## 1 makeCacheMatrix create a special object that 
## set a matrix, set its inverse and provide function to get
## both values
## 2. cacheSolve use the object makeCacheMatrix to set the
## matrix and compute its inverse.The inverse matrix is cached
## so in the next execution the cached value is returned.

## The function makeCacheMatrix create a special "matrix" object. 
## It returns a list of functions that: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xinverse <<- inverse
    getinverse <- function() xinverse
    list(set=set, get=get, setinverse=setinverse,
         getinverse=getinverse)
}

## The cacheSolve function input is and object of class
## makeCacheMatrix. 
## The function check whether the inverse matrix has been
## already computed (cached). If the inverse is in the cache
## returns its value. If the inverse is not in the cache,
## compute it, cache its value and return it..

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minverse <- x$getinverse()
    if(!is.null(minverse)) {
        message ("Getting cached data for matrix inverse")
        return(minverse)
    }
    mdata <- x$get()
    minverse <- solve(mdata, ...)
    x$setinverse(minverse)
    minverse
}
