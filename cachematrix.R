## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.
## These functions, makeCacheMatrix() and cacheSolve(), are used to cache 
## the inverse of a matrix  
##
## makeCacheMatrix() creates a special matrix, that is, creates an object 
## 	that can cache its inverse
## 1. set           set the value of the matrix  
## 2. get           get the value of the matrix 
## 3. setinverse    set the value of inverse of the matrix 
## 4. getinverse    get the value of inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL						#initialize n to NULL to say there is no inverse
	set <- function(y = matrix()){		#used to set a new matrix without creatin a new object
		m<<-NULL					# '<<-' is used to chage the variables in the parent environment
		x<<-y
	}
 	get <- function() x					#Returns the matrix
	setinverse <- function(inverse) m <<- inverse 	#stores the inverse matrix
	getinverse <- function() m				#returns the inverse matrix

      list( get = get,set =set,				#list of functions to be used by cacheSolve  
             setinverse = setinverse,
             getinverse = getinverse )
}


## This function checks if the inverse has already been calculated,
##	if so, gets the calculated inverse via getinverse(), skiping the computation,
##    otherwise calculates the inverse and sets the value of the inverse 
##    in the cache via the setinverse()

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse ()			#tries to get the inverse matrix
      if(!is.null(m)) {				#if the inverse matrix exists	
          message("getting cached data")	#uses the cached inverse matrix and skips the 
          return(m)	       		#inverse computation
      }						
      data <- x$get()				#the inverse matrix does not exist, gets the matrix to be inverted
      m <- solve(data, ...)			#inverts the matrix	
      x$setinverse(m)				#stores the inverted matrix in the cache
      m
}



