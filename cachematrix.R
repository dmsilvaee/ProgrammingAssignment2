# There are two functions. The first one store a special "matrix" object
# in the cuputer memory as a list where:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of a square matrix, only for square and inversible matrices (det != 0)
# 4.  get the value of the inverse of a square matrix, only for square and inversible matrices (det != 0)

# The second function calculates the inverse of the special 
# "matrix" created with the first function. However, it first 
# checks to see if the inverse has already been calculated. 
# If so, it `get`s the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the 
# data and sets the value of the inverse in the cache 
# via the `setsolve`function.

#This function creates a special "matrix" object
#that can cache its inverse, only for square and inversible matrices (det != 0).
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL 
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.   
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
