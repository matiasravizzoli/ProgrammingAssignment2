## This function creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to
## set the value of the vector
## Get the value of the matrix
## set the value of the inverse (solve)
## set the value of the inverse (solve)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
       setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}




## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
              
      
}

## Execution Example:
## MakeCache <- makeCacheMatrix()
## MakeCache$set(matrix(rnorm(25000000), 5000,5000))   ##Creates a huge Matrix to test the functions
## cacheSolve(MakeCache)