## Put comments here that give an overall description of what your
## functions do

# Pretty much just copied the sampple. 
# makeCacheMatrix creates a list of functions that can manipulate the cache of the inverse, it takes no inputs
# cacheSolve takes a matrix (x) and first checks to see if there is a cached inverse and returns it if there is one, 
#if not it then retruns solve(x) and caches thay inverse

## Write a short comment describing this function
#This function is only for creating a list of functions, do not change the value of x, 
#individual functions described below

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #A variable for the inverse cache
        set <- function(y) { #will take a matrix (y) and then sets it to be the global varibale x
                x <<- y
                inv <<- NULL #also sets the inverse cache to null
        }
        get <- function() x #returns the matrix set by set()
        setinv <- function(inverse) inv <<- inverse #sets the inverse casche to whatever you want
        getinv <- function() inv #gets the cached inverse
        list(set = set, get = get,  #return a list of the above functions, you use the functions in this list, not makeCacheMatrix()
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
#Takes a matirx and then uses the varibles in the list created by makeCacheMatrix() to cache or use cache of inverse, 
# and returns an inverse

cacheSolve <- function(x, ...) {#takes x, the list created by makeCacheMatrix()
	##NOTE, you must first set the matrix by using set, e.g. x$set(matrix)
         ## Return a matrix that is the inverse of 'x'
         #actually the function does not return inverse of x, but of a martix you set before hand
        inv <- x$getinv()#checks for cached matrix
        if(!is.null(inv)) { #returns cached inverse if there is one
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # if no cache then these lines take the matrix and return solve(matrix)
        inv <- solve(data, ...)
        x$setinv(inv) #caches inverse
        inv #returns inverse
}