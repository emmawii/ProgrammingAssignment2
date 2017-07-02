## The 2 functions below cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	 i<<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(inv){
    i<<-inv
  }
  getInverse<-function(){
    i
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function above. If the inverse has already been calculated, the cacheSolve function retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
