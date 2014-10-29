# The makeCacheMatrix() function is a function that takes a matrix as an argument and creates a special vector.. in this case, the vector is a list of functions.
# makeCacheMatrix( returns a list of functions: get (which gets the vector), set (which sets the vector), setinv (which sets the inverse of the matrix) and getinv (which gets the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        
        # Create a variable called inv that will hold the inverse of the input matrix. Initialize it to NULL
        inv <- NULL
        
        # The set function takes a matrix as input.        
        set <- function(y) {
                # Store the input matrix in the global (other than current) environment, so that it can be accessed from another environment when required.
                x <<- y
                inv <<- NULL
        }
        # The get function gets the matrix that was stored in the global environment when required. So, when you call this function, it just returns the stored matrix
        get <- function() x
        # The setinv function takes a matrix as an argument, computes it's inverse using solve function, and stores it in global environment variable (inv), so that it can be accessed when required
        setinv <- function(z) {
                s <- solve (z)
                inv <<- s      
        }
        # The getinv function returns the inverse of the matrix that was computed and stored in the global environment varibale (inv)
        getinv <- function() {
                inv
        }
        # The makeCacheMatrix () function returns a list of functions: set, get, getinv, setinv... each of which can be accessed by a calling function.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The cacheSolve() function computes the inverse of a matrix using the solve function. But, before it does that, it checks to see if an inverse for the matrix has been computed before and available in the cache. If so, it brings the value from the cache. 
# This function takes a matrix argument, for which inverse has to be computed.

cacheSolve <- function(x, ...) {
        
        # Obtain the inverse of matrix that was computed by makeCacheMatrix(). Since the function makeCacheMatrix() returns a list, you can access items of the list using the $ operator
        inv <- x$getinv()
        # Check if inverse has been calculated before (and stored in cache). If it is in cache, then print the message that it is getting it from cached data, and return the inverse of the matrix from the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If inverse of matrix in question has never been calculated, print the message that it is a fresh calculation, and 
        
        else {
                message("fresh calculation")
                # use the get() function to obtain the value of matrix, solve function to get the inverse of matrix, and update the cache with the inverse using the setinv() function
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                # Finally, return the inverse of the matrix
                inv
        }
}
