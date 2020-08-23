## Assignment: Caching the Inverse of a Matrix
## Lexical Scoping
### Scoping is the set of rules that govern how R looks up the value of a symbol.
####There are four basic principles behind R’s implementation of lexical scoping:
    
    #### * name masking
    #### * functions vs. variables
    #### * a fresh start
    #### * dynamic lookup

library(matlib)
makeCacheMatrix <- function(x = matrix()) {  
    if (ncol(x) == nrow(x) && det(x) != 0) {  # a fresh start
        inv <- NULL                       
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
                                        
        get <- function() x
        setinverse <- function(solve) inv <<- solve   
        getinverse <- function() inv
        list(set = set,                         # returning a list()
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
    }else{
        return(message("The matrix can not be inverted."))
    }
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    inv <- x$getinverse()
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)  # calculate the inverse
    x$setinverse(inv)
    inv
}

## doing tests
A <- makeCacheMatrix(matrix(c(2, 5, 1, 3),ncol=2,nrow=2))
cacheSolve(A)
A$get()
A$getinverse()
