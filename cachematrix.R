##Assignment: Caching the Inverse of a Matrix

##Creates makeCacheMatrix function similar to example makeVector at https://class.coursera.org/rprog-011/human_grading/view/courses/973492/assessments/3#human_grading/view/courses/973492/assessments/3/submissions

## The function creates a special "matrix" object that can cache its inverse.


## The makeCacheMatrix function creates a list containing fuction to 
##a) set the values of the matrix
##b) get the values of the matrix
##c) set the inverse values of the matrix
##d) get the inverse values of the matrix

makeCacheMatrix <- function(x = matrix()) { 
##creates empty matrix (x)
  m <- NULL 
  ##sets m to inital value NULL
  
  set <- function(y){ 
    x <<- y 
    m <<- NULL 
    # sets function  where  is assignet argument y and m is re-set to NULL 
  } 
  
  
  get <- function() x 

  ##  get function returns the matrix
  
  setInverse <- function(solve) m <<- solve 
  ## sets function were prevoius m value is replaced with new solved value 
  
  getInverse <- function() m 
  ## returns the Inverse m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
  ## creates the list of function
} 



## Creates cacheSolve function similar to cachemean example at https://class.coursera.org/rprog-011/human_grading/view/courses/973492/assessments/3#human_grading/view/courses/973492/assessments/3/submissions


##cachesolve function returns the inverse matrix that was created with the function makeCachematrix above




cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse() 
  ## assigns m the latest inverse value 
  
  if(!is.null(m)){ 
    message("getting inversed matrix") 
   
    return(m) 
    ##creates function were m is returned if it is not null
  } 
  
  
  data <- x$get() 
  m <- solve(data, ...) 
  
  x$setInverse(m) 
  
  m  
  ##computes, caches and returs the inverse value m if it was notreturned under previous function 
}

## tests the functions accprding to suggestion at https://class.coursera.org/rprog-011/forum/thread?thread_id=105 (Rob de Beier)
## Example run:
##
## > source ("cachematrix.R")
## > my_matrix = matrix( c(1, 1, 3, 1), nrow=2, ncol=2)
## > x <- makeCacheMatrix(my_matrix)
## > x$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    1    1
## > cacheSolve(x)
## [,1] [,2]
## [1,] -0.5  1.5
## [2,]  0.5 -0.5
## > cacheSolve(x)
## getting inversed matrix
## [,1] [,2]
## [1,] -0.5  1.5
## [2,]  0.5 -0.5
## >
## > my_matrix = matrix( c(1, 1, 2, 4, 1, 3, 2, 2, 1), nrow=3, ncol=3)
## > x$set(my_matrix)
## > x$get()
## [,1] [,2] [,3]
## [1,]    1    4    2
## [2,]    1    1    2
## [3,]    2    3    1
## > cacheSolve(x)
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333
## > cacheSolve(x)
## getting inversed matrix
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333
## > 

## Finishes the assigment with still slightly confused