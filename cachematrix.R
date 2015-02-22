#This two following funtions have the same structure the makeVector and cachemean.


makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL  #initialize the matrix in wich the inverse will be stored.
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
   }
  get <- function() x # return the input matrix
  set_inv <- function(inverse) m_inv <<- inverse # set the inversed matrix
  get_inv <- function() m_inv # return the inversed matrix
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)  # return a list that contains these functions
}

#The previous list will be using in the following cacheSolve funtion,
#depending of the state of data inside of matrix.

cacheSolve <- function(x, ...) {
  m_inv <- x$get_inv() # get the inversed matrix to m_inv variable.
   if(!is.null(m_inv)) { # verify the data inside the matrix. The first time the data will be null.
    message("getting cached data")
    return(m_inv) 
  }
  data <- x$get() # if not,This funtion will calculate the ineverse using "SOLVE" function
  m_inv <- solve(data) 
  x$set_inv(m_inv)
  m_inv 
}

#testing the functions
#probe<- matrix(rnorm(100),4,4) #Probe is a square matrix and non singular, for it exist inverse
#MatrixCached <- makeCacheMatrix(probe) # The matrix object is created
#MatrixCached$get()
#test<-cacheSolve(MatrixCache)#it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inv function.
#MatrixCached$get_inv()
#test

