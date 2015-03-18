These two function create, store and recall a matrix and its inverse in/from cache   
2 
 
3 
 
4 ## makeCacheMatrix creates custom matrix type capable of running four functions 
5 ## set stores the matrix in cache, get recalls the matrix 
6 ## setInverse and getInverse do the same but for the inverse of the original matrix 
7 
 
8 makeCacheMatrix <- function(x = matrix()){     
9   m <- NULL 
10   set <- function(y){ 
11     x <<- y   
12     m <<- NULL #store matrix in cache  
13   } 
14   get <- function() x #get matrix 
15   setInverse <- function(solve) m<<- solve #set inverse matrix 
16   getInverse <- function() m #get inverse matrix 
17   list(set = set, get = get, 
18        setInverse = setInverse, 
19        getInverse = getInverse)  ## create list of functions 
20 } 
21 
 
22 ## cacheSolve take a custom matrix type created by the makeCacheMatrix function 
23 ## and calculates the inverse matrix of it 
24 ## but first it checks to see if the calculation has been done before 
25 ## if it has been done before it recalls the data from the cache. If it has not been done  
26 ## before it calculates the inverse matrix then store it in the cache 
27 
 
28 cacheSolve <- function(x, ...) { 
29   ## Return a matrix that is the inverse of 'x' 
30   m <- x$getInverse()                 #query the x matrix's cache 
31   if(!is.null(m)){                    #if there is a cache the inverse has been previously calculated 
32     message("getting cached data")    # sent message indicating this is just cache  
33     return(m)                         # return the cache   
34   } 
35   data <- x$get()                     # get the matrix used by makeCacheMatrix function  
36   m <- solve(data, ...)               # calculate the inverse of the matrix 
37   x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function 
38 } 
