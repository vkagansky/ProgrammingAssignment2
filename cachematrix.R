## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix caches (potentially time-consuming) computations
## of the inverse matrix (as well as the average value of matrix)

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             getInverse = getInverse,
             setInverse = setInverse,
             setmean = setmean,
             getmean = getmean)
}




## Return a matrix that is the inverse of 'x'


## Write a short comment describing this function

## cacheSolve checks to see if the invesed matrix has already been calculated.
## If so, returns it; otherwise caclulates the inversed matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}