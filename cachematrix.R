## Assignment week 3 in R programming
## Write a short comment describing this function
## The function " makeCacheMatrix" is to create a special matrix which can cache the inverse of it 
## in order to save running time. 
 

makeCacheMatrix <- function(x = matrix()) {
    vectors <-NULL
    set <- function(y){
    x <<- y
    vectors <<- NULL
    }
    get <- function() {x}
    setvector <- function(inverse) {vectors<<- inverse}
    getvector <- function(){vectors}
    list(set=set, get=get, setvector=setvector, getvector=getvector)
}

## The function "cacheSolve" is to compute the inverse of the matrix from above.
## IF the Matrix has been calculated If so, it gets the inverse from the cache and 
## skips the computation.

cacheSolve <- function(x, ...){
      vectors <- x$getvector ()
      if(!is.null(vectors)) { 
          message("getting cached data")
          return(vectors)
      }
      data <-x$get()
      vectors <-solve(data,...)
      x$setvector(vectors)
      vectors
} 
