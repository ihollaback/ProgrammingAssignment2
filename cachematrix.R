## The functions below calculate the inverse of a matrix that is invertable and cache the data.
## If the maxtrix you are trying to invert has been inverted before, instead of calculating it again
## the function will call it from cache.

## makeCacheMatrix creates a list that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
            x <<- y
            s <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(solve) s <<- solve
        getmatrixinverse <- function() s
        list(set = set, get = get,
        setmatrixinverse = setmatrixinverse,
        getmatrixinverse = getmatrixinverse)

}


## cacheSolve calculates the inverse matrix if it has not been calculated before and will cache the inverse matrix.
## If the inverse matrix being calculated has been calculated before then cacheSolve retrieves the inverse matrix and prints that it was obtained from cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getmatrixinverse()
        if (!is.null(s)) {
                message("getting cached data")
                return (s)
        }
        data <- x$get()
        s <-solve(data, ...)
        x$setmatrixinverse(s)
        s
        
}

## Sample run:
## > x = rbind(c(1, 4), c(4, 1))
## > s = makeCacheMatrix(x)
## > s$get()
##      [,1] [,2]
## [1,]    1    4
## [2,]    4    1
## > cacheSolve(s)
##             [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667
## > cacheSolve(s)
## getting cached data
##             [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667




