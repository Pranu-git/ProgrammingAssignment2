## Put comments here that give an overall description of what your
## functions do

## a function that creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get <-function() x
  setinverse<-function(inverse)
    inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

  
  
  
  
  list(set,get, setinverse,getinverse)

}


## function to find inverse if it has not been computed before

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        matrix_to_invert<-x$get()
        inv<-solve(matrix_to_invert,...)
        x$setinverse(inv)
        inv
}
