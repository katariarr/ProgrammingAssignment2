## makeCacheMatrix converts the input matrix to a list of functions. cacheSolve
    ## makes use of these functions to calculate the inverse of the matrrix and 
    ## store it as cached data. If the inverse had already been computed using this 
    ## set of functions, it simply fetches the answer from the cached data.

    ## converts the given matrix into a list of functions, to be used by cacheSolve
    ## to give out the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) m <<- i
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

    ## Uses the CacheMatrix to get the required functions. Computes and stores the
    ## inverse in cached data and pulls the data from there in further usage.

cacheSolve <- function(x, ...) {
                                    ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting inverse from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
