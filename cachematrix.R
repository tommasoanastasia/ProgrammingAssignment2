##I'm creating a pair of functions that cache the inverse of a matrix
##I use the first function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
    opp <- NULL
    set <- function(y) {
        x <<- y
        opp <<- NULL
    }
    get <- function() x
    setOpp <- function(solve) opp <<- solve
    getOpp <- function() opp
    list(set = set, 
         get = get,
         setOpp = setOpp,
         getOpp = getOpp)
    
}

## The second function returns the inverse of the matrix if it does not already exist
## Otherwise, the function returns a messagge 'getting cached data'

cacheSolve <- function(x, ...) {
    opp<- x$getOpp()
    if(!is.null(opp)){
        message('getting cached data')
        return(opp)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    opp <- solve(data)
    x$setOpp(opp)
    opp
}
##I test the functions

my_matrix = matrix(c(13,2,47,48), nrow=2, ncol = 2, byrow = FALSE)

special_matrix=makeCacheMatrix(my_matrix)

cacheSolve(special_matrix)
