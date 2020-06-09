## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    
    get <- function() x
    setinverse <- function(matrix.inv) {
        xi <<- matrix.inv
    }
    getinverse <- function() {
        xi
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xi <- x$getinverse()
    if (!is.null(xi)) {
        message("getting cache data")
        # return the cache value, which is the inverse of the matrix 'x'
        return(xi)
    }
    
    # retrieve the original matrix
    data <- x$get()
    # compute the inverse of matrix 'data'
    xi <- solve(data)
    # update the inverse cache in object x
    x$setinverse(xi)
    # return the inverse of the matrix
    xi
}


## Testing
## 1) create a matrix 'm' as follow
##    m <- matrix( c(4, 2, 2, 2, 3, 1, 2, 1, 3), nrow=3, byrow=TRUE)
## 2) create a special matrix using matrix 'm' above
##    sm <- makeCacheMatrix(m)
## 3) call funcation 'cacheSolve' to compute the inverse of the matrix 'm' 
##    inside the special matrix 'sm'
##    im <- cacheSolve(sm)
## 4) output the inverse of matrix m
##    im
## 5) find the product of m x im
##    im %*% m


