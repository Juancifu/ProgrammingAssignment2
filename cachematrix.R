##Caching the inverse of a matrix

##This function creates a list that contains 4 functions: set, get, setInv, getInv.

makeCacheMatrix<-function(x=matrix()){
  invx<-NULL ##Where the result of inversed matrix is stored
  set<-function(y){
    x<<-y
    ##The operator '<<-' is used to assign a value to an object in an environment different from the current environment. 
    invx<<-NULL
  }
  get<-function()x ##It returns the inputed matrix
  setinv<-function(inverse)invx<<-inverse ##It sets the inversed matrix
  getinv<-function()invx ##It gets the inversed matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##This next function returns the inversed matrix created with the makeCacheMatrix function.

##But, if the cached inverse is available, this function retrieves it, while if not, it computes, caches, and returns it.

cacheSolve<-function(x,...){
  a<-x$getInv() ##To get the inversed matrix from object x (it will be null if uncalculated)
  if(!is.null(a)) { ##If the inversion result is there
    message("Cached data")
    return(a) ##It returns the calculated inversion
  }
  data<-x$get() ##But if not, get the matrix object
  a<-solve(data) ##So this solves it
  x$setInv(a) ##To set it to the object
  a ##This will return the solved result
}
