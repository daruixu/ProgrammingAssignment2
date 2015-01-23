## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix function takes an inversible matrix as input
#and returns a list of four functions.
#the functions take advantage of the super assignment operator 
#to modify variable value in the parent level instead of the local level

makeCacheMatrix <- function(x = matrix()) {
        inversed_x <- NULL
        set <- function(y) {
                x <<- y
                inversed_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) inversed_x <<- inversed
        getinverse <- function() inversed_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# the cacheSolve takes a spceical matrix created by makeCacheMatrix.
# when called the first time, it will use solve() to inverse the matrix
# when called again, it will return the cached inversed matrix
# and save the cost of inversing a matrix
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        input_matrix <- x$get()
        m <- solve(input_matrix)
        x$setinverse(m)
        m
}
