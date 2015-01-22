# two functions which will cache the inverse of a matrix


# this first function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL   # initializing inverse
  
  # then set matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # then method to get matrix
  get<-function() x
  
  # this next part is to set and get the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  # return the list of methods
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# this next function will retrive the inverse from the cache

cacheSolve <- function(x=matrix(), ...) { 
  m<-x$getmatrix() #  returns inverse of x matrix 
  
  # return the inverse if its set already
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <-x$get()  # get the matrix
  m<-solve(matrix, ...) # calculate the inverse 
  x$setmatrix(m)
  
  # return the matrix whcich is the inverse
  m
}

# testing the function

a <- makeCacheMatrix(matrix(c(1,3,2,4), nrow = 2, ncol = 2))
summary(a)

a$get()

cacheSolve(a)

# the 2nd time we run the function,we get the cached value
cacheSolve(a)
