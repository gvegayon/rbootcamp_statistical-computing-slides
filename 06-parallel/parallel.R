## ---- echo=FALSE, fig.cap="Flynn's Classical Taxonomy ([Introduction to Parallel Computing, Blaise Barney, Lawrence Livermore National Laboratory](https://computing.llnl.gov/tutorials/parallel_comp/#Whatis))", fig.align='center'----
knitr::include_graphics("../fig/flynnsTaxonomy.gif")


## ----03-how-many-cores---------------------------------------------------
parallel::detectCores()


## ----gpu-cpu, echo=FALSE, fig.cap="[NVIDIA Blog](http://www.nvidia.com/object/what-is-gpu-computing.html)", fig.align='center', out.width="400px"----
knitr::include_graphics("../fig/cpuvsgpu.jpg")
nnodes <- 4L


## ----good-idea, echo=FALSE, fig.cap="Ask yourself these questions before jumping into HPC!", fig.align='center', out.width="60%"----
knitr::include_graphics("../fig/when_to_parallel.svg")


## ----parallel-ex-psock, echo=TRUE----------------------------------------
# 1. CREATING A CLUSTER
library(parallel)
nnodes <- 4L
cl     <- makePSOCKcluster(nnodes)    

# 2. PREPARING THE CLUSTER
clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`

# 3. DO YOUR CALL
ans <- parSapply(cl, 1:nnodes, function(x) runif(1e3))
(ans0 <- var(ans))


## ----parallel-ex-psock-cont, echo=TRUE-----------------------------------
# I want to get the same!
clusterSetRNGStream(cl, 123)
ans1 <- var(parSapply(cl, 1:nnodes, function(x) runif(1e3)))

# 4. STOP THE CLUSTER
stopCluster(cl)

all.equal(ans0, ans1) # All equal!


## ----parallel-ex-fork, echo=TRUE, eval = TRUE----------------------------
# 1. CREATING A CLUSTER
library(parallel)

# The fork cluster will copy the -nsims- object
nsims  <- 1e3
nnodes <- 4L
cl     <- makeForkCluster(nnodes)    

# 2. PREPARING THE CLUSTER
clusterSetRNGStream(cl, 123)

# 3. DO YOUR CALL
ans <- do.call(cbind, parLapply(cl, 1:nnodes, function(x) {
  runif(nsims) # Look! we use the nsims object!
               # This would have fail in makePSOCKCluster
               # if we didn't copy -nsims- first.
  }))

(ans0 <- var(ans))


## ----parallel-ex-fork-cont, echo=TRUE------------------------------------
# Same sequence with same seed
clusterSetRNGStream(cl, 123)
ans1 <- var(do.call(cbind, parLapply(cl, 1:nnodes, function(x) runif(nsims))))

ans0 - ans1 # A matrix of zeros

# 4. STOP THE CLUSTER
stopCluster(cl)


## ----what-did-you-said, out.width="60%", echo=FALSE----------------------
knitr::include_graphics("../fig/what-did-you-said.gif")


## ----parallel-ex-mclapply, echo=TRUE, eval = TRUE------------------------
# 1. CREATING A CLUSTER
library(parallel)

# The fork cluster will copy the -nsims- object
nsims  <- 1e3
nnodes <- 4L
# cl     <- makeForkCluster(nnodes) # mclapply does it on the fly

# 2. PREPARING THE CLUSTER
set.seed(123) 

# 3. DO YOUR CALL
ans <- do.call(cbind, mclapply(1:nnodes, function(x) runif(nsims)))

(ans0 <- var(ans))


## ----parallel-ex-mclapply-cont, echo=TRUE--------------------------------
# Same sequence with same seed
set.seed(123) 
ans1 <- var(do.call(cbind, mclapply(1:nnodes, function(x) runif(nsims))))

ans0 - ans1 # A matrix of zeros

# 4. STOP THE CLUSTER
# stopCluster(cl) no need of doing this anymore


## #include <omp.h>

## #include <RcppArmadillo.h>

## 
## // [[Rcpp::depends(RcppArmadillo)]]

## // [[Rcpp::plugins(openmp)]]

## 
## using namespace Rcpp;

## 
## // [[Rcpp::export]]

## arma::mat dist_par(const arma::mat & X, int cores = 1) {

## 

##   // Some constants

##   int N = (int) X.n_rows;

##   int K = (int) X.n_cols;

## 

##   // Output

##   arma::mat D(N,N);

##   D.zeros(); // Filling with zeros

## 

##   // Setting the cores

##   omp_set_num_threads(cores);

## 

## #pragma omp parallel for shared(D, N, K, X) default(none)

##   for (int i=0; i<N; ++i)

##     for (int j=0; j<i; ++j) {

##       for (int k=0; k<K; k++)

##         D.at(i,j) += pow(X.at(i,k) - X.at(j,k), 2.0);

## 

##       // Computing square root

##       D.at(i,j) = sqrt(D.at(i,j));

##       D.at(j,i) = D.at(i,j);

##     }

## 

## 

##   // My nice distance matrix

##   return D;

## }


## ----dist-dat, dependson=-1, echo=TRUE, cache=TRUE-----------------------
# Simulating data
set.seed(1231)
K <- 1000
n <- 500
x <- matrix(rnorm(n*K), ncol=K)

# Are we getting the same?
table(as.matrix(dist(x)) - dist_par(x, 4)) # Only zeros


## ----dist-benchmark, echo=TRUE, cache=TRUE-------------------------------
# Benchmarking!
rbenchmark::benchmark(
  dist(x),                 # stats::dist
  dist_par(x, cores = 1),  # 1 core
  dist_par(x, cores = 2),  # 2 cores
  dist_par(x, cores = 4), #  4 cores
  replications = 10, order="elapsed"
)[,1:4]


## ----future, echo=TRUE, collapse=TRUE, cache=TRUE------------------------
library(future)
plan(multicore)

# We are creating a global variable
a <- 2

# Creating the futures has only the overhead (setup) time
system.time({
  x1 %<-% {Sys.sleep(3);a^2}
  x2 %<-% {Sys.sleep(3);a^3}
})

# Let's just wait 5 seconds to make sure all the cores have returned
Sys.sleep(3)
system.time({
  print(x1)
  print(x2)
})


## ---- echo=FALSE, dev='jpeg', dev.args=list(quality=100), fig.width=6, fig.height=6, out.width='300px', out.height='300px'----
set.seed(1231)
p    <- matrix(runif(5e3*2, -1, 1), ncol=2)
pcol <- ifelse(sqrt(rowSums(p^2)) <= 1, adjustcolor("blue", .7), adjustcolor("gray", .7))
plot(p, col=pcol, pch=18)


## ----simpi, echo=TRUE----------------------------------------------------
pisim <- function(i, nsim) {  # Notice we don't use the -i-
  # Random points
  ans  <- matrix(runif(nsim*2), ncol=2)
  
  # Distance to the origin
  ans  <- sqrt(rowSums(ans^2))
  
  # Estimated pi
  (sum(ans <= 1)*4)/nsim
}


## ----parallel-ex2, echo=TRUE, cache=TRUE---------------------------------

# Setup
cl <- makePSOCKcluster(4L)
clusterSetRNGStream(cl, 123)

# Number of simulations we want each time to run
nsim <- 1e5

# We need to make -nsim- and -pisim- available to the
# cluster
clusterExport(cl, c("nsim", "pisim"))

# Benchmarking: parSapply and sapply will run this simulation
# a hundred times each, so at the end we have 1e5*100 points
# to approximate pi
rbenchmark::benchmark(
  parallel = parSapply(cl, 1:100, pisim, nsim=nsim),
  serial   = sapply(1:100, pisim, nsim=nsim), replications = 1
)[,1:4]



## ----printing-and-stop, cache=TRUE---------------------------------------
ans_par <- parSapply(cl, 1:100, pisim, nsim=nsim)
ans_ser <- sapply(1:100, pisim, nsim=nsim)
stopCluster(cl)


## ---- echo=FALSE---------------------------------------------------------
c(par = mean(ans_par), ser = mean(ans_ser), R = pi)


## ----thanks, out.width="300px", echo=FALSE-------------------------------
knitr::include_graphics("../fig/speed.gif")


## ----session, echo=FALSE-------------------------------------------------
sessionInfo()

