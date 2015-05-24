#Part 1:  write a function that creates a special matrix object that can cache its inverse.
#using form given in assignment and required titling of variables...

#sets and gets the value of the matrix, then gets the inverse if the matrix is square and invertible using the "solve" function

makeCacheMatrix <-function(x=matrix()){
  m<-NULL
  set <- function (y) {
    x<<-y
    m<<-NULL
  }
  get <-function () x
  setmatrix <-function (solve) m<<-solve  
  getmatrix <-function() m
  list(set=set, get=get, setmatrix = setmatrix, getmatrix=getmatrix)
}

#Part 2:  write a function that computes the inverse of the matrix from part 1.  if inverse is already calculated, it should retrieve the value from part 1.
#using form as given in assignment and specified titling of variables...
cacheSolve <-function(x=matrix(), ...) {
  m <-x$getmatrix()
  #this checks to see if the matrix has already been inverted... if so, returns the inverse.  if not, calculates
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  matrix <-x$get()
  m<-solve(matrix, ...)
  m
}
