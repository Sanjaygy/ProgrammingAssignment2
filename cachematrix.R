## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix makes a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function (y) {
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setInversee<-function(inverse) i<<-inverse
  getInverse<-function() i
  list(set=set, get=get,
       SetInverse=SetInverse,
       getInverse=getInverse)
}


## CacheSolve function gives the inverse of the special "matrix" computed 
##by makeCacheMatrix function

cacheSolve <- function(x) {
  i<-x$getInverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)){
    return(i)
  }
  Mymatrix<-x$get()
  i<-solve(Mymatrix) ##computes the inverse of the matrix "x"
  x$setInverse(i)
  i
}
