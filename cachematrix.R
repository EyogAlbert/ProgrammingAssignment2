## MakeCashematrix is a function that gets a matrix as input and stores its value in the cache
## the invm line assigns to invm(which is the inverse of the matrix x) a NULL value
## In the set function the assign operator gives the value y(Input argument) to x in the parent environment
## assigns NULL to invm in the parent environment
## The get function gets the value of x from parent environment
## The setinverse function computes the inverse of x
## The getinverse retrieves the value of invm
## The list created on the last line gives names to the different functions so that they can be called using $

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function (y){ 
                x<<-y
                invm <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) invm <<- solve
        getinverse <- function() invm
        list(set= set, get=get, setinverse =setinverse, getinverse= getinverse)
}



## The cacheSolve function gets the matrix x checks whether the value of its inverse is cached and if so retrieves it else computes it 
## attempts to retrieve the inverse using x$getinverse()
##Then it checks to see whether the result is NULL.
## if the value retrieved is null, then the if function retrieves the input object and then returns it's inverse
cacheSolve <- function(x, ...) {
        invm <- x$getinverse()
         if (!is.null(invm)){
                print("getting cached data")
                return(invm)
         }
        dat <- x$get()
        invm <- solve(dat, ...)
        x$setinverse(invm)
        invm
}
