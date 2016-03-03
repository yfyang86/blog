## definition
## adjmat[i,j] i->j

#' @title Hyperlink-Induced Topic Search
#' @description Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a link analysis 
#' algorithm that rates Web pages, developed by Jon Kleinberg. The idea behind Hubs and Authorities 
#' stemmed from a particular insight into the creation of web pages when the Internet was originally 
#' forming; that is, certain web pages, known as hubs, served as large directories that were not 
#' actually authoritative in the information that they held, but were used as compilations of a broad 
#' catalog of information that led users direct to other authoritative pages. In other words,
#' a good hub represented a page that pointed to many other pages, and a good authority represented 
#' a page that was linked by many different hubs.
#' URL: \url{https://en.wikipedia.org/wiki/HITS_algorithm}
#' @param adjmat is the adjacent matirx with adjmat[i,j] i->j
#' @param iter_times is the maxum iteration time
#' @param tol is the L2 torlerance
#' @param control is a control list.
#' \itemize{
#' \item analytic is T/F indicates use SVD or not, default =F
#' \item trace is the trace trigger(T/F), default =F
#' }
#' @return p.hub is the hub value that satisfies 
#' @return p.auth is the auth value
#' @examples 
#' hits_alg(control=list(trace=T)) 
hits_alg <- function(
  ## adjmat[i,j] i->j
  adjmat = matrix(sample(
    0:1,Length*Length,replace = T, 
    prob = c(.9,.1)),100,100),
  iter_times = 100,
  tol = 1e-12,
  control=list(trace=FALSE,analytic=FALSE)
  ){
  controls=list(trace=FALSE,analytic=FALSE)
  uuu = intersect(names(control),names(control))
  if (length(uuu)>0) for (i in  uuu) controls[[i]] = controls[[i]] || control[[i]]
  # clean: row col == all zero
  diag(adjmat) = 1
  Length1 = dim(adjmat)[1]
  Length2 = dim(adjmat)[2]
  # initial
  p.auth = rep(1,Length1) # p.auth is the authority score of the page p
  p.hub = rep(1,Length2)  # p.hub is the hub score of the page p
  
  #' incomingNeighbors is the set of pages that link to p
  #' @param vet is a index (not vector)
  #' @param adh is the adjacent matrix
  incomingNeighbors <- function(vet, adjmat){
    which(adjmat[,vet]>0)
  }
  #' outcomingNeighbors is the set of pages that link to p
  #' @param vet is a index (not vector)
  #' @param adh is the adjacent matrix
  outcomingNeighbors <- function(vet, adjmat){
    which(adjmat[,vet]>0)
  }
  # start for loop:  run the algorithm for k steps
  for( k in 1:iter_times){ ## TODO: update all authority values first
    p.hubold = p.hub
    norm_factor = 0;
    for (p in 1:Length){
      p.auth[p] = sum(p.hub[incomingNeighbors(p,adjmat)]); # use inner for loop
      norm_factor = norm_factor + (p.auth[p])^2; # calculate the sum of the squared auth values to normalise
    }
    
    p.auth =   p.auth/sqrt(norm_factor)   #  update the auth scores 
    norm_factor=0;        # reset normal factor
    for (p in 1:Length){  # then update all hub values
      p.hub[p] = sum(p.auth[outcomingNeighbors(p,adjmat)]);
      norm_factor = norm_factor + (p.hub[p])^2;
    }
    p.hub =   p.hub/sqrt(norm_factor)   # normalise the hub values
    
    err = sqrt(sum( (p.hub-p.hubold)^2))
    if (controls$trace) cat(k,"-change:",err,'\n')
    if (err < tol) break;
  }
  
  return(list(p.hub,p.auth));
}