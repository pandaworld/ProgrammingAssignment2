##   Matrix inversion is usually a costly computation and the following functions 
##   can be used to compute the inverse of a matrix and cache the result  
##   so that it can be used rather than computing repeatedly. 


##   makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##   Assumption for this assignemt is that the matrix supplied is always invertible.
##   This function creates a special "matrix", which is really a list 
##   containing a function to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {     # create a matrix object and sub functions
  m <- NULL                                     
  set <- function(y) {                          # sets values for variable in parent environment 
    x <<- y                                     # assign y to variable x in the parent environment
    m <<- NULL                                  # assign m to NULL in parent environment
  }
  get <- function() x                           
  setmatrix <- function(solve) m <<- solve       
  getmatrix <- function() m
  list(set = set, get = get,                    # return cached inverse
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##  cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  above. If the inverse has already been calculated (and the matrix has not changed),
##  then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()                           
  if(!is.null(m)) {                            ## check if the inverse was cached before
    message("getting cached matrix")           ## if it is cached, return the cached result
    return(m)
  }
  cachematrix <- x$get()
  m <- solve(cachematrix, ...)                 ## if it was not cached before, solve and 
    x$setmatrix(m)                             ## compute the inverse and return the value
  m
}
