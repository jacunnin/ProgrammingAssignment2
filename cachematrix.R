## Create two functions
## The first function creates the matrix
## The second function should solve for the inverse of this matrix

## Create a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solve for the inverse of the function

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    #solve calculates the inverse of a matrix
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

made <- makeCacheMatrix(matrix(c(4,1,3,1),2,2))
solved <- cacheSolve(made)
solved
