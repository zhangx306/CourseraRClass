## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a list that contain four funcs for a given matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  get<-function(){x}
  
  set<-function(y){
    x<<-y
    inv<<-NULL
    }
  
  getInv<-function() {inv}
  
  setInv<-function(new_inv) {
    inv<<-new_inv
  }
  
  list(get=get,set=set,getInv=getInv,setInv=setInv)

}


## Write a short comment describing this function
## Either retrieved the cathed inverse of a matrix, 
## or compute the inverse when the cached value is N/A

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getInv()
    if(!is.null(inv)){
      print("Retriving cached inverse")
      return(inv)
    }
    
    matrix<-x$get()
    inv<-solve(matrix,...)
    x$setInv(inv)
    
    return(inv)
    
}
