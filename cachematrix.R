## The function cacheSolve below returns the inverse of a matrix x

## The function cacheSolve checks the value of makeCacheMatrix to see if that 
## function already has stored the solution in memory before it independently
## makes the calculation

makeCacheMatrix <- function(x = matrix()) {

     ## set the matrix

     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }

     ## get the value of the matrix

     get <- function() x

     ## create the inverse of the matrix using the solve function

     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function returns the inverse of the matrix x by first checking the
## result of makeCacheMatrix for a solution then using the solve function 
## independently if no solution in the cache

cacheSolve <- function(x, ...) {

     ## Return a matrix that is the inverse of 'x'

     ## get the inverse of the matrix

     i <- x$getinverse()

     ## check if there is a matrix stored in the cache as a result of 
     ## the makeCacheMatrix function above

     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }

     ## if nothing in the cache, calculate the inverse of the matrix

     else {
         invmatrix <- x$get()
         i <- solve(invmatrix, ...)

         ## set the inverse of the matrix

         x$setinverse(i)
         i
     }
}
