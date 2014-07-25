## these functions makes caches of matrixes and inverse them

makeCacheMatrix <- function(x) {  ## this function makes a cache matrix
  i <- NULL    ## where i is the values of the inversed matrix
  setvalues <- function(y) {    ## this function resets the values of i 
    x <<- y
    i <<- NULL
  }
  getvalues <- function() x ## this function gets the assigned values of x and returns them
  setcache <- function(inversed) i <<- inversed ## this function cached i to setinversedvalues
  getcache <- function() i   ## this function gets the cached values of i and returns them
  list(set = setvalues, get = getvalues, 
       setinversedvalues = setcache,
       getinversedvalues = getcache)
}
cacheSolve <- function(x) {      ## this function either returns an old inverse matrix, or calculates a new inverse matrix if new parameters are set
  i <- x$getinversedvalues()   ## where i is the values of the inverse matrix
  if(!is.null(i)){    ## when a new i is not set the function gets the cached i
    message("getting cached data") ## prints message
    return(i) ## returns the cached i
  }
  data <- x$getvalues() ## gets the newly set values and assigns it to data
  i <- data %*% solve(data) ## calculates the inverse matrix of data
  x$setinversedvalues(i) ## sets the newly calculated inverse matrix to x
  i ## returns the newly calculated inverse matrix
}
