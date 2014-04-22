
##This function creates a especial matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) m<<-solve
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



##This function computes the inverse of the especial matrix
##In case has already been calculated it will retrieve from cache
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    return(m)
  }
  data<- x$get()
  m<-solve(data,..)
  x$setinverser(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
