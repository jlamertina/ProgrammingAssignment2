## Module overview: calcualte the mean of a vector, or retrieve the mean from memory

## Functions in this module include:

## makeVector() - creates a special "vector", which is really a list containing a function to: 
##    set / get vector
##    set / get mean of the vector

## cachemean() - calculates the mean of the special "vector" created with the above function; 
##    or retrieves the mean from cache if its already been calculated.

## Author: Coursera R Programming 2 Assignment description

## Sample execution: 
##    x <- makeVector(c(1,2,4)))   # Must assign to a memory spot (x) for caching of mean
##    cachemean(x)                 # Now we calcuate and cache the mean
                                   
## Note that executing "cachemean(makeVector(c(1,2,4)))" will calculate but not cache the mean

makeVector <- function(x = numeric()) {
  # Input to this function is a numeric vector   e.g c(1,2,4)   
    
  # 1. initialize the mean
    m <- NULL 
    
  # 2. define four inner functions: set, get, setmean, and getmean
    
    set <- function(y) {  # set is an inner function 
        #  Input to this function is a vector to assign to x
        #  Return a NULL for makeVector.m  (the calculated mean)
        #  As this function is defined inside makeVector, makeVector is the parent environ
        x <<- y      #  <<-  assign a value to the parent environ's x var (i.e. makeVector.x)
        m <<- NULL   #  <<-  assign a value to the parent environ's m var (i.e. makeVector.m)
    }
    
    get <- function() x  # get is an inner function returning the input vector, x
  
    setmean <- function(mean) m <<- mean   # setmean is an inner function, assigns a newly calculated mean to m
    
    getmean <- function() m    # getmean is aan inner function returning m, the stored mean

    # construct and return a list by calling upon the four functions: set, get, setmean, getmean
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
    # Input to this function is the stored vector object, x, created by makeVector()
    m <- x$getmean()   # call upon the functions of the object, including getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}