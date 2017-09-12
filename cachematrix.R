##BP/12/09/2017


## makeCacheMatrix() -- generates the cachematrix object with its environemnt
##                   -- generates the function list for accesing the cachematrix object

makeCacheMatrix <- function(M =matrix()){ ## M is the input matrix
  INV<<-NULL              ##Inverse matrix initialization
  
  ## set() -- Sets the M matrix to the new matrix m and INV matrix back to NULL  
  set <- function(m) {
    M <<- m               ## The matrix M is set to the new matrix m
    INV <<- NULL          ##Matrix inverse initialized as null
  }
  ##Accessor Matrix M
  get <- function() M    
    
  
  ##SetInv()--Sets the INV matrix by calling solve()
  setInv<-function(solve) INV <<- solve 
  ##Accessor Matrix INV
  getInv<-function() INV  
  
  
  ##Generate the function list naming the functions to allow use of $ on the matrix object
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function stores the inverse matrix in a cacheMatrix environemnt 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix associated with 'x'
  INV <- x$getInv()  
  ## Test to see if there is an INV allready stored 
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  } 
  ## thos part is executed only if there is no INV in the Cache
  data <- x$get()
  INV <- solve(data, ...)
  x$setInv(INV)
  INV
}


## The following code can be used for testing if uncomented

  # mat=matrix(c(2, 4, 3, 1, 5, 7, 4, 1, 5),nrow=3 ,ncol=3)
  # myCM<-makeCacheMatrix()  ##create CacheMatrix object with no set numeric matrix
  # myCM$get()               ##Test to see if matrix is null
  # myCM$set(mat)            ##Set matrix
  # myCM$get()               ##Test to see if matrix was set
  # myCM$getInv()            ##Test to see if matrix inverse is still null
  # cacheSolve(myCM)         ##Call to cache solve
  # myCMImat<-myCM$getInv()  ##Call to getINv() stored in myCMI
  # 
  # mat%*%myCMI              ##Test M*M^(-1)=I non diagonal  entries all > 1e-16
  # cacheSolve(myCM)         ##Test to see if cacheSolve does return the cache value if it exist
                           
 
 
