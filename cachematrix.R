## The following pair of functions calculate the inverse of a matrix and cache
## it in a way that, if the input matrix does not change, its inverse is
## retrieved from the cache instead of being recalculated.

## The first function creates an object that has a matrix as a formal argument
## and builds a set of functions returning them to the parent environment
## as a list. This function is able to cache the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x
  setsolve<-function(solve)s<<-solve
  getsolve<-function()s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

## The second function computes the inverse of the previously created
## special "matrix" or retrieves it from cache if it has been previously
## calculated.

cacheSolve <- function(x, ...) {
  s<-x$getsolve()
  if(!is.null(s)){
    message("Getting cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setsolve(s)
  s
}