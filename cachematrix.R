## These two functions allow the user to store a matrix and cache it's
## inverse for quicker retrieval

## MakeCacheMatrix - This function creates a list of sub functions used by
## cacheSolve

makeCacheMatrix<-function(x=matrix()){
  m <- NULL
  ## set changes the matrix used by the function if one is not available
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get returns the matrix
  get <- function() x
  ## setinverse sets the inverse matrix
  setinverse <- function(inverse) m <<- inverse
  ## getinverse returns the inverse matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## cacheSolve calculates the inverse of the desired matrix
## If an inverse has already been calculated, the cached inverse 
## will be returnedrather repeating the same calculation

cacheSolve<-function(x,...){
  ## Get the vlaue of the inverse
  m <- x$getinverse()
  ## If the inverse has already been calculated return cached inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix, obtain the inverse using "solve", then set inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
