makeCacheMatrix <- function(x = matrix()){
#caches the inverse of the matrix
	
	#assigns the value of m to NULL
	m <- NULL
	
	#sets the value of the matrix
	set <- function(y) {
		#assigns input arguement(y) to x
		x <<- y
		#clears values of m that has been cached
		m <<- NULL
        }
      
	#gets the value of the matrix
	get <- function() x
      
	#sets value of the inverse matrix
	setmatrix <- function(matrix) m <<- matrix
      
	#gets the inverse matrix
	getmatrix <- function() m
      
	#gives names to functions above
	list(set = set,get = get,setmatrix = setmatrix,getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
#solves for the inverse of the matrix
	
      	#runs getmatrix function from makeCacheMatrix function
	m <- x$getmatrix()
      
	#determines if there is a cached inverse matrix
	if(!is.null(m)) {
		message("getting cached data")
      
	        #returns the cached inverse matrix
		return(m)
      }
	
      #if there is no cached invere matrix,
      #runs get function from makeCacheMatrix function
      data <- x$get()

      #solves inverse matrix
      m <- solve(data, ...)

      #runs setmatrix function with m as arguement 
      x$setmatrix(m)

      #print inverse matrix
      m
}
