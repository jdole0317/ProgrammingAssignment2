## Program assignment 2: create two functions and run a test of them
## functions ultimately result in an inverted matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) m <<- solve
  getsolve<-function()m
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverser has already
## been calculated then cachSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}

test<-matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3)
mat<-makeCacheMatrix(test)
cacheSolve(mat)
