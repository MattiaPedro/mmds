# PageRank

# Include the library Matrix for dealing with sparse matrices
library('dplyr')
library('Matrix')

# Data of the problem: teleport probability = .2, error = 10^-6
beta <- .8
eps <- 10 ^ -6

# load the data
t_load <- system.time(
  edge_list <- read.csv("~/Google Drive/Data Science/Coursera/MiningMassiveDatasets/web-Google.csv", header = T, sep = "\t", comment.char = "#") + 1
)
rank <- numeric(max(edge_list))

# Add a column to the data reporting for every edge (i,j) the degree of hte node i
t_matrix <- system.time({
  ext_list <- edge_list %>% arrange(FromNodeId) %>% group_by(FromNodeId) %>% mutate(Value = 1 / length(FromNodeId))

# build the transition matrix. ToNode = lines, FromNode = columns, degree of the FromNode as value
  M <- sparseMatrix(ext_list$ToNodeId, ext_list$FromNodeId, x = ext_list$Value)

# some of the node aren't actually in the dataset. Those are the nodes i for which neither column i nor row i has a value different from zero.
# reducing the transition matrix eliminating those nodes
  keep <- rowSums(M) + colSums(M) != 0
  M <- M[keep, keep]
})

# number of actual nodes
N <- dim(M)[1]

# correction vector for teleportation
t_pr <- system.time({
  corr <- rep((1 - beta) / N, N)

# initializing rank vector and error
  r <- rep(1/N, N)
  err <-1

# actual PageRank
  while(err > eps){
    rp <- beta * M %*% r + corr
    rp <- rp + (1 - norm(rp, "1")) / N
    err <- norm(r - rp, "1")
    r <- rp
  }
})

rank[keep] <- as.numeric(r)

rank[100]






