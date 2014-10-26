## Functions makeCacheMatrix and cacheSolve are used to implement an extended matrix
## representation that can be used to cache the inverse of the matrix after computation.
## The extended representation can be used to reduce computational cost when the inverse of
## matrix is used multiple instances without the need to implement ad hoc caching strategies. 
##
## makeCacheMatrix creates an instance of the extended matrix
## cacheSolve calculates the inverse of extended matrix using cahced value if available
##
## matrix is supposed to be invertible


## Function makeCacheMatrix creates an extended matrix representation that supports
## caching of the matrix inverse. In order to take advantage of cached computations
## the function cacheSolve should be used to compute the inverse of the extended 
## matrix.
##  
## x1<-makeCacheMatrix(x) returns extended representation of matrix x 
## 
## x1$set(y) changes matrix value and clears the cached inverse
## x1$get()  gets the underlining matrix value
## x1$setinverse(inverse) sets the value of the cached inverse
## x1$getinverse() returns the value of the cached inverse
##


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## Function cacheSolve calculates the inverse of an extended matrix representation
## obtained from makeCacheMatrix, it takes advantage of extended matrix caching 
## capabilities to store the inverse and retrieve it when available.
##  
## makeSolve(x) returns the inverse of extended cahecing matrix x 
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

