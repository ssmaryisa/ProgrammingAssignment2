## Functions for creating and using inverted matrices which caching ability

##The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mtx_orig = matrix()) {
  
  # initialize the stored inverse value to NULL
  mtx_invert <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    mtx_orig <<- y
    mtx_invert <<- NULL
  }
  
  #get the value of the matrix
  get <- function() mtx_orig
  
  #set the value of the solve
  set_invert <- function(solve) mtx_invert <<- solve
  
  #get the value of the solve
  get_invert <- function() mtx_invert
  
  # return a list containing all functions defined above
  list(
    set = set, 
    get = get,
    set_invert = set_invert,
    get_invert = get_invert
  )
}

#The cacheSolve function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated, then the 
#cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mtx_orig, ...) {
  
  #get inverse matrix
  mtx_invert <- mtx_orig$get_invert()
  
  #if inverse matrix exists, check if already cached
  #if yes, return cached inverse
  if(!is.null(mtx_invert)) {
    message("Getting cached inverse matrix")
    return(mtx_invert)
  }
  
  #if not, get matrix
  m_i <- mtx_orig$get()
  
  #compute inverse of matrix
  mtx_invert <- solve(m_i, ...)
  
  #cache inverse of matrix
  mtx_orig$set_invert(mtx_invert)
  
  #return inverse matrix
  mtx_invert
}