## The following functions will cache and generate the inverse of a supplied matrix


##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y = matrix) {
    ## <<- assigns the variable to the parent environment
    ## in this case, it is the global environment
      ## For some reason demanded that I expicitly define its input
      ## as a matrix, otherwise it kept assuming it was a vector
    x <<- y
      ## x is the input vector, stored in global memory
    m <<- NULL
      ## a flag that changes depending on value storage
  }
  get <- function() x
    ## returns the value of x the matrix
  setIMATRIX <- function(solve) m <<- solve
    # Here I'm assigning a function to use the function solve
  # to compute the inverse (THANK YOU R)
  # This operation depends on whether another variable is a NA 
  # (serving as a flag, essentially)
  
  getIMATRIX <- function() m
  list(set = set, # the input matrix will be assigned as defined
       get = get, # same with the retreival of the input 
       setIMATRIX = setIMATRIX,  # solving the matrix
       getIMATRIX = getIMATRIX)
  
}

## This function returns a matrix that is the inverse of 'x'
##  If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x = matrix(),...){
  m <- x$getIMATRIX()
  ## performs a function on m, and assigns its return as m in the local environment
  
  if(!is.null(m)) {
    message("getting cached data--in this case, the matrix!")
    ## The condition where m == NA is when 
    return(m)
  }
  dataMATRIX <- x$get()
  m <- solve(dataMATRIX, ...)
  x$setIMATRIX(m)
  m
    }
