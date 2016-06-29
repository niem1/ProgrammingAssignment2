## Computing a matrix inverse is computationally intensive. So in situations where 
## the same inverse can be used repeatedly, it is advantageous to store the inverse 
## once computed so that it can be used again without recomputing it.
##


## makeCacheMatrix is a function that takes a square object and sets and gets the matrix
## also sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a matrix, checks to see if the inverse has already been
## computed.  If it has it retrieves the inverse from cache and teminates.  If
## it has not it then computes the inverse and stores and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  }

##Sample run

# > trial<-rbind(c(1, 2), c(3, 4))
# > t<-makeCacheMatrix(trial)
# 
#   > t$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# > t1<-cacheSolve(t)
# > ti
# Error: object 'ti' not found
# > t1
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > t1<-cacheSolve(t)
# getting cached data.
# > t1
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
#   > t1
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
