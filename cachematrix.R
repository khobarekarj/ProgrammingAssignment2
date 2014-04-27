## This file contains functions to calculate an inverse of a matrix. 
## The special feature is that the function saves the inverse of a matrix in a cach.
## which can be used if the inverse of the matrix is to be calucalted again.

## makeCacheMatrix is a function which contains 4 functions (set, get, setinverse and getinverse).
## these functions help in getting the inverse of a matrix from the cash if it exists.
makeCacheMatrix <- function(x = matrix()) {
    ## x is the input matrix
    ## i is the matrix where the inverse of x can be stored. 
    ## the default value of i should be null if the set function is not called.
     i <- NULL
        ## set function sets the input_matrix as x and initializes i as null
        set <- function(input_matrix) {
            x <<- input_matrix
            i <<- NULL
        }
        ## get functions helps in displaying matrix x
        get <- function() x
        ## setinverse function sets the input to the function as i
        setinverse <- function(inverse) {
            i <<- inverse
        }
        ## getinverse function helps in displaying inverse of x i.e. i
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function is used to get the inverse of a matrix.
## if the inverse exists in the cache then it is retrived from the cache
## else the inverse is calculated and stored in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from cache
        i <- x$getinverse()
        ## if i not null then there is an inverse matrix present in cache and returning that caches matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if i is null then the inverse is calculated with the solve function in r
        data <- x$get()
        i <- solve(data)
        ## setting the inverse in the cache so that it can be used in future
        x$setinverse(i)
        ## return the inverse
        i
}


## Test example of how to run the function

##  >  source("cachematrix.R")
##  >  a <-makeCacheMatrix(rbind(c(1, 2), c(3, 4)))
##  >  a$set(rbind(c(1, 2), c(3, 4)))
##  >  a$get()
##  >  cacheSolve(a)

## run cacheSolve(a) to check that the value is been taken from the cache or is being computed.
