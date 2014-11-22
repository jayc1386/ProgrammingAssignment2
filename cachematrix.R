## The file contains two functions that work in conjuction with each
## other. Because calculating the inverse of a matrix is a costly
## computation, it may be beneficial to store the inverse of a matrix
## that you are working with rather than constantly running the same
## process to calculate it. By storing it in the cache, it would only 
## require a call to the stored inverse matrix rather than running a
## calculation each time you would like to retrieve its value.

## This function creates a special "vector", which is really a list 
## containing four functions that:
## 1. store matrix x
## 2. retrieve matrix x
## 3. store the inverse of matrix x
## 4. retrieve the inverse of matrix x
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

## This function first checks if the result of the makeCacheMatrix 
## function has indeed inversed and cached the inverse of a matrix. 
## If it has, then it will return the inverse. If it has not, then 
## it will calculate the inverse of the original stored matrix.
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
