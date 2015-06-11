## The first function creates a special matrix object that 
## can cache the object's inverse; the second function checks the cache
## first, if not found, it will go ahead and calculate the inverse then 
## calls the first function to cache it. 

# Matrix Inverse: http://www.mathsisfun.com/algebra/matrix-inverse.html
# To test:
# c=rbind(c(1, -1/4), c(-1/4, 1))
# cacheSolve(c)
# solve(c) %*% c
# You should get the Identity Matrix
    

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    # check for cache
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    ## Return a matrix that is the inverse of 'x'
    m
}


