## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix get and sets the value and inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
      ## intialize a variable to store the inverse of matrix
      inv <- NULL 
      
      ## get
      ## gets the already present matrix
      get <- function() x 
      
      ## set
      set<- function(y){
            x <<- y ## set the new matrix
            inv <<- NULL ## clear the variable which was cached already
      }
      
      ## getinverse
      getinverse <-function() inv
      
      ## setinverse
      setinverse <- function(i) inv <<- i
      
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
      
}


## cacheSolve will create the inverse of a given matrix if not already 
## present in the case

cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()
      
      ## check if inverse is already computed and return it
      if(!is.null(inv)){
            message("getting cached inverse")
            return(inv)
      }
      
      ## if no inverse is cached new inverse is calculated and returned
      m <- x$get()
      
      inv <- solve(m , ...)
      
      x$setinverse(inv)
      
      ## Return a matrix that is the inverse of 'x'
      inv
}