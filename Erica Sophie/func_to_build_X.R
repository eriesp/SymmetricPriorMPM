## funzione per costruzione matrici velocemente

# agent_feat: vector of features of the agent (k_d * 1)
# product_feat: matrix of features of the products, each row is a product, the columns are the features

build_X <- function(agent_feat, product_feat){
  
  p = dim(product_feat)[1]
  
  # intercette
  identity <- matrix(0,p,p)
  diag(identity) <- 1
  
  # agent specific part
  kd = length(agent_feat)
  
  X_d <- matrix(0,p,p*kd)

  for (i in 1:p){
    X_d[i, ((i-1)*kd +1):(i*kd)] = agent_feat
    # X_d = matrix(X_d,p,p*kd)
    X_d = matrix(unlist(X_d), ncol = p*kd, byrow = FALSE)
  }
  
  # product specific part
  X_a = as.matrix(product_feat)
  
  X.i <- cbind(identity,X_d,X_a)
  
  return(X.i)
}

save(build_X, file="function_build_X.RData")

