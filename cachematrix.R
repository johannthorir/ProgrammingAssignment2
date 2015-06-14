##


## Create a cached Matrix object (list of methods with closure)

makeCacheMatrix <- function(x = matrix()) {
  ## the cached inverse...
  inv <- NULL 
  
  ## returns the matrix
  get <- function() x
  
  ## sets the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## sets the inverse of the matrix.
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## returns the chached inverse.
  ## I would implement the solution here, instead
  ## of in a separate function... but hey...
  getinverse <- function() {
    inv
  }
  
  ## cached matrix is a list of methods.
  list(
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse
  )
}


## Solve a cached matrix object.
cacheSolve <- function(x,...) {
  inv <- x$getinverse() 
  
  ## if it has already been done, return the cached solution.
  if(!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  
  ## othewise calculate the solution.
  mat <- x$get()
  inv <- solve(mat)
  
  ## and store it in the chachematrix.
  x$setinverse(inv)
  
  ## and return the solution.
  inv
}
