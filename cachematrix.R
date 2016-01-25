## This function takes in a matrix and creates a list of the functions 
##needed for the rest. 

makeCacheMatrix <- function(myMatrix = matrix()) {
  cachedInverse <- NULL
  set <- function(inputMatrix){
    myMatrix <<- inputMatrix
    cachedInverse <<- NULL
  }
  get <- function(){
    myMatrix
  }
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getInverse <- function (){
    cachedInverse
  }
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## This function finds the inverse of the matrix, and checks it
## it has been found before and stored in cache. If the value can be found in 
## cache, the function returns the cached value instead of
## calculating it again. 

cacheSolve <- function(x, ...) {
  
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)){
    message("Getting cached data")
    return(cachedInverse)
  }
  
  data <- x$get()
  cachedInverse <- solve(data)
  x$setInverse(cachedInverse)
  cachedInverse
  
}
