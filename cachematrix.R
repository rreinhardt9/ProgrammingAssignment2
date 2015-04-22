## makeCachedMatrix create a special matrix list object with getters and setters for it's inverse
## cahceSolve uses the cached inverse if available, otherwise it calculates and sets the inverse

## Create special cached matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Find inverse of matrix using special cached matrix above

cacheSolve <- function(x) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}

## For testing
testMatrix <- matrix(runif(100,1,80), 10, 10)