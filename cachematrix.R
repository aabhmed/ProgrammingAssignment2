1 ## The code creates the following two functions a) makeCacheMatrix and b) cacheSolve 
2  
3 ## Purpose of the makeCacheMatrix function is to create a custom matrix capable executing four functions
4 ## The following functions are exectued by makeCacheMatrix
5 ## (i)   set stores the matrix in cache
6 ## (ii)  get recall the matrix
7 ## (iii) setInverse stores the inverse of the original matrix
8 ## (iv)  getinverse recall the inverse of the original matrix  
 
 
9 makeCacheMatrix <- function(x = matrix()){     
10   m <- NULL 
11   set <- function(y){ 
12     x <<- y   
13     m <<- NULL #store matrix in cache  
14   } 
15   get <- function() x #get matrix 
16   setInverse <- function(solve) m<<- solve #set inverse matrix 
17   getInverse <- function() m #get inverse matrix 
18   list(set = set, get = get, 
19        setInverse = setInverse, 
20        getInverse = getInverse)  ## create list of functions 
21   } 
22 
 
23 ## Pupose of the cacheSolve function is to process the custom matrix type created by the makeCacheMatrix function 
24 ## and calculate the inverse matrix of the custom matrix 
25 ## before inverse matrix is create a check is performed to determine if the calculation has been done before 
26 ## if the calculation has been done, data is recalled from the cache. If the calculation has not been performed 
27 ## the inverse matrix is calculated and stored in the cache 
28 
 
29 cacheSolve <- function(x, ...) { 
30   ## Return a matrix that is the inverse of 'x' 
31   m <- x$getInverse()                 #query the x matrix's cache 
32   if(!is.null(m)){                    #if there is a cache the inverse has been previously calculated 
33     message("getting cached data")    # sent message indicating this is just cache  
35     return(m)                         # return the cache   
36   } 
37   data <- x$get()                     # get the matrix used by makeCacheMatrix function  
38   m <- solve(data, ...)               # calculate the inverse of the matrix 
39   x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function 
40   } 
