## These two function work together to store the value of the
## inverse of a matrix, so that it need not be calculated 
## multiple times.

## makeCacheMatrix creates a list that contains a function to  
## 1. set the value of a matrix
## 2. get the value of a matrix whose value has been set
## 3. set the value of a matrix's inverse
## 4. get the value of a matrix's inverse

## Note that makeCacheMatrix needs a square invertible matrix
## as an input.

makeCacheMatrix <- function(x = matrix()) {
    
    # first clear any m's lying around
    m <- NULL
    
    # define 1.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # define 2.
    get <- function() x
    
    # define 3.
    setinverse <- function(solve) m <<- solve
    
    # define 4.
    getinverse <- function() m
    
    # populate the list with functions 1-4. 
    # name these four list items set, get, 
    # setinverse, getinverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve finds the inverse of a matrix. Before
## calculating, it checks to see if the value is already 
## stored in the cache. If already stored, the calculation
## is skipped.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    # check to see if the inverse has already been 
    # calulated for this value. If it has, don't do any 
    # calculations, just return the inverse.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get the matrix whose inverse we want, set as 'data'
    data <- x$get()
    
    # calculate the inverse
    m <- solve(data)
    
    # set the value of the inverse to our cached list.
    x$setinverse(m)
    
    # return the calulated inverse
    m
}
