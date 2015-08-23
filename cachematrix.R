# makeCacheMatrix takes a matrix as parameter and contains
# a set of functions get, setSolve and getSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

# cacheSolve takes the function makeCacheMatrix as parameter and checks
# if the inverse of the matrix is already cached.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {   # If m is not null the inverse matrix is already cached.
                message("getting cached data")
                return(m)
        }
        # If this is run the matrix was not cached and the following code will be executed where
        # the inverse will be calculated and the inverse cached.
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}

# Example
m <- matrix(1:4, 2)
mi <- makeCacheMatrix(m)
cacheSolve(mi)  # The inverse of m is calculated and returned.
cacheSolve(mi)  # The inverse of m is returned from cache along with the message "getting cached data".
