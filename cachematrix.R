## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list of functions that let me set the value of 
## the matrix (set) and return the value of x (get);and set and get its cached inverse (with setinverse and getinverse functions).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <-function(mat){
    x <<-mat
    inv <<-NULL
  }
  get <- function() x
  setinverse<-function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function allows me calculate the inverse (with the function Solve) of the matrix, using the cache matrix obtained 
## with the makeCacheMatrix function. With this function, if the inverse of the matrix is cached in the previous function, it will return it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
