## My functions comprised two functions: "makeCacheMatrix", and "cacheSolve".
## You can cache an matrix using "makeCacheMatrix" function and
## You can get inversed matrix using "cacheSolve" function. 

## To cache the matrix, you should input the matrix 
## into round bracket '()' behind makeCacheMatrix such as
## "x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))".

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                inverse <- solve(x)
                i <<- inverse
                }
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## if you operate "cacheSolve(x)", "cacheSolve" function calculate the inverse 
## of the matrix previously you input in '()'. However, If you operate it again,
## the function may print message such as getting cached data

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}