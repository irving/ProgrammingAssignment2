## Programming Assignment 2 for Coursera R programming course
## Dave Vandervort;  4/23/2015
## The code herein was adapted from code supplied to describe the assignment
## Original found at https://class.coursera.org/rprog-013/human_grading/view/courses/973494/assessments/3/submissions

## Make a vector that caches the inverse of the supplied matrix
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


# Return cached or new solution to inverse of a square matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } 
    
    # implicit else statemen
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
