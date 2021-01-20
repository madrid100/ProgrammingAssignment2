## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv
##Libarary(MASS) is used to calculate inverse for non squared as well as square matrices
  inv ##Return a matrixthat is the inverse of "x"
  library(MASS)
  makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL        #initializing inverse as NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function()x          #function to get the matrix x 
    setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
      inver<-ginv(x)
      inver%%x        #function to obtain inverse of the matrix
    }
    list(set= set, get=get,setinv = setinv, getinv=getinv)
  }
  
  
  ## Write a short comment describing this function
  ##This is used to get cache data
  cacheSolve <- function(x, ...) ##get cache data
  {
    inv<-x$getinv()
    if(!is.null(inv)){ ##checking whether inverse is null
      message("getting cached data!")
      return(inv) #returns inverse value
    }
    data<-x$get()
    inv<-solve(data,...)  #calculates inverse value
    x$setinv(inv)
    inv ##Return a matrix that is the inverse of "x"
  }