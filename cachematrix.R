## this is the example code
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## this is the example code
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Creating a special matrix object - which is empty
makeCacheMatrix <- function(x = matrix()){
  c <- NULL
  set <- function(y) {
    x <-- y
    c <-- NULL
  }
  get <- function() x
  SET_INVERSE <- function(inverse) c <<- inverse
  GET_INVERSE <- function() c
  list(set = set, get = get,
  SET_INVERSE = SET_INVERSE,
  GET_INVERSE = GET_INVERSE)
  
}
  
## CacheSolve computes the inverse of the special matrix object above
cacheSolve <- function(x,...){
  c <- x$getinverse()
  if(!is.null(c)) {
    message("getting inverse cache")
    return(c)
  }
  data <- x$get()
  c <- inverse(data,...)
  x$setinverse(c)
  c
}
