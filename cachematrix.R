# Create a vector that is a list that contains functions that...
# set the value of the vector to a matrix in the parent environment
# get the value of the vector as a matrix
# set the value of the inverse matrix to the parent environment (cache)
# get the value of the inverse matrix
makeCacheMatrix <- function (mat1 = matrix(...)) {
        
        # First, initiate a null vector in parent env
        matInv <- NULL
        
        # This method takes in a matrix for which inverse is to be computed
        # it stores it to the null vector in the parent env using <<-
        set <- function(mat2) {
                mat1 <<- mat2 # set matrix in parent env to input
                matInv <<- NULL # set matrix inverse in parent env to null
        }
        
        # Return input matrix to a computing function that calls
        get <- function() {
                mat1 
        }
        
        # Caching the computed inverse to parent environment!
        setinverse <- function(putInvToCache) {
                
                matInv <<- putInvToCache
        }
        
        # Return parent environment's value of matrix inverse
        getinverse <- function() {
                
                matInv
        }
        
        # Define an instance of makeCacheMatrix function;
        # it is a list containing its defined methods
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# This function calculates the inverse of the matrix vector created above. 
# Before doing that it checks the parent environment to see if the inverse
# value has been cached prior to this run. If yes, it simply returns the cached
# value. If not, then it will compute the inverse, store it in cache, and display it.

cacheSolve <- function(x, ...) {
        
        # Get the cached inverse value stored in parent environment by calling
        # getinverse() method (list element) of the makeCacheMatrix vector
        cachedMatInv <- x$getinverse() 
        
        # if not Null, then it has already been computed, return it and end
        if(!is.null(cachedMatInv)) {
                message("getting cached data")
                return(cachedMatInv)
        }
        
        # if we're here, inverse was not cached. 
        # get the matrix to compute
        mat1 <- x$get()
       
        # compute inverse using solve()        
        cachedMatInv <- solve(mat1)
        
        # store the inverse to the parent environment
        # by calling the setinverse method above
        x$setinverse(cachedMatInv)
        
        # display the computed inverse
        cachedMatInv
}