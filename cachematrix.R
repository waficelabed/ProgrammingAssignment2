#-------------------------------------------------------------------
#In This Part we define a special object of type makecaheMatrix
#makecacheMatrix takes an argument of type matrix
#and implements the default geters and setters 
#get and set retrives and sets the content of the obeject matrix 
#this object also implements two functions : 
#setInverse and getInvrese : two compute and return the inverse of object matrix
#-------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
  r<-NULL #Initializing the matrix to an empty matrix
  set<-function(y) #implementing the setter function 
  {
    x<<-y #setting free variable y from outside environement
    r<<-NULL
  }  
  get <- function() x #implementing the getter 
  setInverse <-function(inverse)r<<-inverse # defining setInverse()
  getInverse <-function() r #defining getInvese , retuns the inverse of a matrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
#------------------------------------------------------------------
#computes the inverse of a matrix if no cache exists 
#if the value of the inverse is already present then no need to compute
#------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  r<-x$getInverse()
  if(!is.null(r))#checks for the existence of cache
  {
    message("getting cached data");
    return(r)
  }
  data<- x$get()#gets matrix data
  r<- solve(data,...)#solve the inverse
  x$setInverse(r)#sets the inverse to the newly computed inverse
  r #returns the inverse
}