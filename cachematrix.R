## Put comments here that give an overall description of what your
## functions do
## These two functions find the inverse of a matrix and cache the result. This may save some computing time
## for very large matrices, for instance a 10000 x 10000 matrix

## Write a short comment describing this function
## The function makeCacheMatrix returns a list. There are four elements in the list. The first is a function to set
## the matrix, the second is a function to get the matrix. The third and fourth elements are functions to set and get
## the inverse of the matrix respectively.
## Example: x <- makeCacheMatrix(matrix(round(runif(100,min=-1,max=1)*10,0),nrow=10))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The function cacheSolve finds the inverse of the matrix and returns it. If the inverse is allready found (i.e. cached) than the cached
## inverse matrix is returned. The function prints the warning "getting cached data" if the cached value is returned.
## Example:     cacheSolve(x) #First time: returns the inverse of the matrix
##              cacheSolve(x) #Second time: prints the warning "getting cached data", and returns the inverse from cache

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
