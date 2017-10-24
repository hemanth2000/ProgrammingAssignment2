## This function will set the inverse of the matrix present in
## the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv<<-inverse
  getinverse<- function() inv 
  list(set = set, get = get,
           setinverse = setinverse,getinverse=getinverse)

}


## cacheSolve function will search for the inverse of the matrix
## in cache and if it is not found then it will 
## calculate the inverse of the matrix and will set this inverse 
## value for further use.

cacheSolve <- function(x, ...) {
         inv<-x$getinverse()
    if(!is.null(inv)){
       message("getting cached data")
       return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinverse(inv)
    inv
}
