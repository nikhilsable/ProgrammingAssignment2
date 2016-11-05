## makeCacheMatrix() gets a square matrix (x) and 
## generates a list (to set/get matrix, and setmatrix/getmatrix its inverse)
## which acts as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  mt<-NULL
  set<-function(y){
    x<<-y
    mt<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) mt<<- solve
  getmatrix<-function() mt
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## for cacheSolve the input x is the outut from makeCacheMatrix (the list)
## The function returns the inverse(mt) of the original matrix

cacheSolve <- function(x, ...) {
  mt<-x$getmatrix()
  # below if{} statement checks whether mt(inverse matrix) has been 
  ## calculated and is not null. If TRUE, returns the cached inversed matrix(mt).
  if(!is.null(mt)){
    message("getting cached data")
    return(mt)
  }
  matrix<-x$get()
  ## below statement is where we are solving(inverse) for original matrix x
  mt<-solve(matrix, ...)
  x$setmatrix(mt)
  mt
}
