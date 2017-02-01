## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list( set=set, 
              get=get, 
              setinverse=setinverse, 
              getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Sample run:

# > x = matrix(1:4,2,2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
#         
