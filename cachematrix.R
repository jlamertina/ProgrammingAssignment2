## Module overview: Compute the inverse of a square "matrix", or retrieve from memory.
##   ("Matrix inversion is usually a costly computation and there may be a benefit to caching
##   rather than computing the inverse repeatedly."  Coursera R Programming Assignment 2)

## Functions in this module include:
##   makeCacheMatrix() 
##   cacheSolve() 

## Sample execution for invertible 2x2 matrix:
##    m <- matrix(c(1:4),nrow=2,nrow=2)     # Must assign matrix to a memory spot (m) for caching
##    n <- makeCacheMatrix(m)             # Send cached matrix to makeCacheMatrix
##    inv <- cacheSolve(n)              # Now solve for the inverse and cache it
##    i <- m %*% inv                  # product of m and its inverse should be identity matrix
##    inv2 <- cacheSolve(n)         # calling cacheSolve again shows "getting cached inverse"
##    i <- m %*% inv2             # product of m and its inverse should be identity matrix

## Sample invertible 3x3 matrix:
##    m <- matrix(c(1,0,1,2,4,0,3,5,6),nrow=3,ncol=3)

## Author: JL, July 2015

## makeCacheMatrix() - cache an invertible square "matrix" object 

makeCacheMatrix <- function(x = matrix()) {
    # Assumption: input is a square, invertible matrix
    
    # 1. initialize the matrix inverse
    x_inv = NULL
    
    # 2. set and get functions for the matrix
    setX <- function(y) {
        x <<- y   # x is a var in the outer function, makeCacheMatrix, so we must use notation <<-
        x_inv <<- NULL  # we're setting new values for the matrix; so null the inverse
    }
    getX <- function() x
    
    # 3. set and get functions for the matrix inverse
    setX_inv <- function(inverse) x_inv <<- inverse   
    getX_inv <- function() x_inv
    
    # 4. construct and return a list by calling upon the four functions above
    list(setX = setX, getX = getX, setX_inv = setX_inv, getX_inv = getX_inv)
}

## cacheSolve() - compute the inverse of the "matrix" supplied by makeCacheMatrix(); or if the 
##    computation has already been done, retrive the inverse from cache

cacheSolve <- function(mat, ...) {
    # Input to this function is the stored matrix object, mat, created by makeCacheMatrix
    
    inver <- mat$getX_inv()   # check if the inverse is already calculated (i.e. in memory)
    if (!is.null(inver)) {
        message("getting cached inverse")
        return(inver)
    }
    # since the inverse is null, we need to calculate it
    m <- mat$getX()        # get the stored matrix    
    inver <- solve(m)      # solve() calculates the matrix inverse
    mat$setX_inv(inver)    # store the inverse
    inver                  # return the inverse
}
