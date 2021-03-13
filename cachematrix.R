makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  #Setting i as NULL initializes it as an object within the makeCacheMatrix environment to be used by later code in the function
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #The set function assigns the input argument to the x object in the parent environment, and assigns the value of NULL to the i object in the parent environment. This clears any value of i that had been cached by a prior execution of cacheSolve.
  get <- function() x
  #Since the symbol x is not defined within get(), R retrieves it from the parent environment.
  setinverse <- function(solve) i <<- solve
  #Since i is defined in the parent environment and we need to access it after setinverse completes, the <<- operator assigns the input argument to the value of i in the parent environment.
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  # This last section assigns each of these functions as an element within a list(), and returns it to the parent environment. Giving everything names means we can use the $ extractor in the next function.
}


cacheSolve <- function(x, ...) {
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  i <- x$getinverse()
  #First, this tries to retrive an inverse; then it checks whether the result is NULL. If the value is not NULL, there's a valid inverse; if there isn't a valid inverse, the function calculates one with solve.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
