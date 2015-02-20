## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list of four functions (set, get, setsolve, getsolve)
## in OOP terms makeCacheMatrix acts like an object
## and the four functions it contains are accessor and mutator methods
## we store the Inverse of the matrix in the variable inv


makeCacheMatrix <- function(x = matrix()) 	
{	
        inv <- NULL			
        set <- function(y) {		
                x <<- y
                inv <<- NULL		
        }
        get <- function() x		
        setsolve <- function(solve) inv <<- solve	
        getsolve <- function() inv			
        list(set = set, get = get,		
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
## This function gets the inverse of the matrix stored in makeCacheMatrix
## and puts it back inside the makeCacheMatrix "object", i.e. caches it. 
## first it uses getsolve() to try to pull the ached value
## if this is not NULL, then then it returns this value
## if it is NULL, then it uses get() to grab the cached matrix
## find the inverse using solve, then uses setsolve() to cache that value
## by storing it back in makeCacheMatrix

cacheSolve <- function(x, ...) 
{
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv				## Return a matrix that is the inverse of 'x'
}
