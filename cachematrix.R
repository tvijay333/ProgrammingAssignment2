## Put comments here that give an overall description of what your
## functions do

## These functions facilates to cache the inverse of matrix if computed 
## early instead of computing repeatedly 

## Write a short comment describing this function
##   This function creates a special matrix object 
makeCacheMatrix <- function(x = matrix()) {
    InvMtx <- NULL
    set <- function(y){
      x <<- y
      InvMtx <<- NULL
    }
    get <- function() x
    setinv <- function(solve) InvMtx <<- solve
    getinv <- function() InvMtx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
  
}


## Write a short comment describing this function
## This function solves matrix inverse and caches if it was computed before 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMtx <- x$getinv()
  if(!is.null(InvMtx)){
    message("Getting cashed Inverse of the given Matrix")
    return(InvMtx)
  }
    
  Mtx_Inp <- x$get()
  InvMtx <- solve(Mtx_Inp, ...)
  x$setinv(InvMtx)
  InvMtx
  
}
