
cache <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set <- function(inverse) inv <<- inverse
  get <- function() inv
  list(set = set, get = get,
       set = set,
       get = get)
}

cacheSolve <- function(x, ...) {
  inv <- x$get()
  if(!is.null(inv)) {
    message("Fetching Cached Matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set(inv)
  inv
}
