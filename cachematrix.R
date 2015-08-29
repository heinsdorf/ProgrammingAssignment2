# The two functions below demonstrate how a function
# can be programmed to cache its result so that it
# doesn't have to do the same computation twice.
# This is important for time-consuming computations, 
# such as matrix inversion.
#
# usage:
#    p = matrix(data = runif(1000000, 1, 10000000), 1000, 1000)
#    pm <- makeCacheMatrix(p)
#    p1 = cacheSolve(pm)   # takes a few seconds the first time
#    ...
#    p2 = cacheSolve(pm)   # later, very fast

#==================================================
# makeCacheMatrix() makes a matrix cacheing object
# which contains the following:
#
# globals:
#   m_cache -- caches the input matrix
#   i_cache -- caches the inverted matrix
#
# methods:
#   set(input_matrix)      
#   input_matrix <- get()
#   set_inverse(m)
#   get_inverse()
# 
makeCacheMatrix <- function(x = matrix()) {
        i_cache <- NULL
        set <- function(y) {
                m_cache <<- y        # <<- (assigns to global context)
                i_cache <<- NULL
        }
        set(x)
        get <- function() m_cache
        set_inverse <- function(inv) i_cache <<- inv
        get_inverse <- function() i_cache
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

#==================================================
# cacheSolve() 
# usage: cacheSolve(x, ...)
# where x is a matrix to invert
#       ... is extra args for solve()
# Returns a matrix that is the inverse of 'x'.
# Uses makeCacheMatrix() to cache and return the result for
# matrix inverses that have already been computed.
cacheSolve <- function(x, ...) {
        i_cache <- x$get_inverse()
        if (!is.null(i_cache)) {
                message("getting cached data")
                return(i_cache)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}

