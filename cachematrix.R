# makeCacheMatrix creates a list containing four functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inver<-NULL
  set<-function(y)
  {
    x<<-y
    inver<<-NULL
  }
  get<-function()
  {
    x
  }
  setinverse<-function(inverse)
  {
    inver<<-inverse
  } 
  getinverse<-function() 
  {
    inver
  }
  
  list(set=set,  get=get, setinverse=setinverse, getinverse=getinverse) 
  
}


##CacheSolve returns the inverse of matrix.
##It first check whether the inverse has already been computated.
##If already computated, it gets the computated data, i.e. Cached data and doesnt computate again.
##If not alredy computated, it computate the inverse, and sets the value in cache.
cacheSolve <- function(x, ...) 
{
  
  inver<-x$getinverse()
  if(!is.null(inver))
  {
    message("getting cached data")
    return(inver)    
  }
  else
  {
    data<-x$get()
    inver<-solve(data,...)
    x$setinverse(inver)
    inver
  }
}
