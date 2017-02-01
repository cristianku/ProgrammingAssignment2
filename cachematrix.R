# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping 


# makeCacheMatrix: 
#         This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: 
#         This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#         If the inverse has already been calculated (and the matrix has not changed), 
#         then the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) M <<- inverse
        getinverse <- function() M
        list( set=set, 
              get=get, 
              setinverse=setinverse, 
              getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        if(!is.null(M)) {
                message("getting cached data.")
                return(M)
        }
        data <- x$get()
        M <- solve(data)
        x$setinverse(M)
        M
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
