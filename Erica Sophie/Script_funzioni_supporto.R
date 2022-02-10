## elenco funzioni che necessito per creare i vari daset


# ---funzione per il jittering-------------------------------------------------
jittering = function(seed, agent_cov, product_cov){
  
  ## income
  # estraggo da -10 a 10 e poi i valori negativi li metto positivi
  set.seed(seed)
  eps = runif(dim(agent_cov)[1], -10, 10)
  agent_cov$Income = agent_cov$Income + eps
  summary(agent_cov$Income)
  agent_cov$Income[which(agent_cov$Income <= 0)] = - agent_cov$Income[which(agent_cov$Income <= 0)]
  
  # scalo la colonna degli income 
  income = agent_cov$Income
  log_inc_1 = log(income + 6)
  income_scaled_log_1 =  2* (log_inc_1 - min(log_inc_1)) / (max(log_inc_1) - min(log_inc_1)) -1
  # summary(income_scaled_log_1)
  
  agent_cov$Income = income_scaled_log_1
  
  n_a = dim(agent_cov)[1]
  
  ## dimensione famiglia
  #sampliamo 1,2,3 con probabilità proporzionali a quelle effettve nel dataset
  set.seed(seed)
  dim_fam = sample(c(1,2,3), n_a, prob = c(0.45,0.45,0.1), replace = TRUE) 
  
  dummy3_4 = ifelse(dim_fam == 2, 1,0)
  dummy5 = ifelse(dim_fam == 3, 1,0)
  
  agent_cov$Fs3_4 = dummy3_4
  agent_cov$Fs5. = dummy5
  
  for (i in 1:p){
    set.seed(seed*p^2)
    eps = runif(n, -.1, .1)
    product_cov[,i] = product_cov[,i] + eps
  }
  
  return(list(agent_cov, product_cov))
}

save(jittering, file = "Funcs_lots_df/jittering.RData")

# parallelize to build list of X.i----------------------------------------------


# funzione per costruire la lista di matrici per il parallelo
build_list_of_X.i = function(i, ID_prod, ID_agent, agent_cov, product_cov){
  
  curr_id <- ID_prod[i]
  X.i <- build_X(agent_cov[which(ID_agent == curr_id),],
                 t(product_cov[i,]))
  return(X.i)
}

save(build_list_of_X.i, file = "Funcs_lots_df/build_list_X.i.RData")


# funzione wrapper per runnare in parallello
build_X.i_wrapper = function(grid_point){build_list_of_X.i(grid_point, ID_product_specific,
                                                           ID_agent_spec, new_agents_cov,
                                                           new_product_cov)}


save(build_X.i_wrapper, file = "Funcs_lots_df/funcs_for_parallel.RData")


# creazione dei coefficienti BETA ----------------------------------------------

coefficienti = function(seed, agent_cov, product_cov){

  mean_price <- colMeans(product_cov)
  
  # intercetta
  set.seed(12)
  error <- rnorm(p, 0, 0.1)
  beta_0 = mean_price - mean(mean_price) + error
  # beta_0
  # cor(mean_price, beta_0)
  
  # coefficienti product specific = delta
  set.seed(seed)
  delta <- runif(1,-1.25,-0.75)
  # delta
  
  # dimensione di csi
  kd <- dim(agent_cov)[2]
  
  # csi deve essere lungo p*kd
  # p*kd
  
  # csi è diviso in triplette, di cui il primo elmento si riferisce all'income,
  # il secondo alla dummy sulla famiglia composta da 3 a 4 persone
  # il terzo alla dummy sulla famiglia composta da 5 perone o più
  
  # income
  csi1 = 5 + c(- 1, -.7, .075, -2, -3, 0.05, 0.065, .075, .1, -.7)
  # csi1
  # cor(mean_price,csi1)
  
  # fam con 3 o 4 componenti
  set.seed(seed)
  csi2 = runif(p,0,1)
  
  # fam con 5 o più componenti
  set.seed(10*seed)
  csi3 = runif(p,0,1)
  
  csi = numeric(p*kd)
  
  for (i in 1:p){
    csi[((i-1)*kd +1):(i*kd)] = c(csi1[i],csi2[i],csi3[i])
  }

  #### beta grosso più grosso ####
  BETA = c(beta_0,csi,delta)
  
  return(BETA)
}

save(coefficienti, file = "Funcs_lots_df/coeff.RData")


# calcolo delle utilities W.i --------------------------------------------------

calcolo_W.i = function(seed, ID_prod, list.X.i, BETA){
  
  choices = numeric(n)
  list.W <- list(0) 
  
  # Estrazione matrice sigma
  identity = diag(p)
  S = identity*(50-p-1)
  
  set.seed(seed)
  sigma <- riwish(50,S)
  
  for (i in 1:n){
    curr_id <- ID_prod[i]
    
    X.i = list.X.i[[i]]
    
    set.seed(seed*i)
    W.i = rmvnorm(1, X.i %*% BETA, sigma)
    list.W[[i]] <- W.i
    
    choices[i] = which(W.i == max(W.i))
  }
  
  return(list(list.W,choices))
}

save(calcolo_W.i, file = "Funcs_lots_df/calcolo_W.i.RData")


# creazione dei vari dataset ---------------------------------------------------

crea_salva_df = function(n, p, k, list.X.i, list.W, choices, BETA){
  
  index = numeric(p*n)
  for (i in 1:n){
    index[((i*p)-p+1):(i*p)] = rep(i-1,p)
  }
  
  colnames = c("beta_0.1","beta_0.2","beta_0.3","beta_0.4","beta_0.5",
               "beta_0.6","beta_0.7","beta_0.8","beta_0.9","beta_0.10",
               "income_t.1","dummy_3_4.1","dummy_5.1","income_t.2","dummy_3_4.2","dummy_5.2",
               "income_t.3","dummy_3_4.3","dummy_5.3","income_t.4","dummy_3_4.4","dummy_5.4",
               "income_t.5","dummy_3_4.5","dummy_5.5","income_t.6","dummy_3_4.6","dummy_5.6",
               "income_t.7","dummy_3_4.7","dummy_5.7","income_t.8","dummy_3_4.8","dummy_5.8",
               "income_t.9","dummy_3_4.9","dummy_5.9","income_t.10","dummy_3_4.10","dummy_5.10",
               "price")
  
  X.i_df = as.data.frame(list.X.i[[1]])
  colnames(X.i_df) = colnames
  
  for (i in 2:length(list.X.i)){
    curr_X.i = list.X.i[[i]]
    colnames(curr_X.i) = colnames
    X.i_df = rbind(X.i_df,curr_X.i)
  }
  
  W_df <- list.W[[1]]
  for (i in 2:length(list.W)){
    W_df <- rbind(W_df, list.W[[i]])
  }
  
  dim(X.i_df)
  
  X.i_df= cbind(index,X.i_df)
  
  Y = numeric(n*p)
  
  for (i in 1:n){
    Y[((i*p)-p+1):(i*p)] = rep(choices[i],p)
  }
  
  X.i_df= cbind(X.i_df,Y)
  
  beta_df = as.data.frame(BETA)
  
  # creo folder e salvo
  
  dir.create(file.path(paste("Datasets/Data_",k,sep = "")), recursive = TRUE)
  
  write.csv(W_df,paste("Datasets/Data_",k,"/W.csv",sep = ""), row.names = FALSE)
  write.csv(X.i_df,paste("Datasets/Data_",k,"/X.csv",sep = ""), row.names = FALSE)
  write.csv(beta_df,paste("Datasets/Data_",k,"/beta.csv",sep = ""), row.names = FALSE)
  
}

save(crea_salva_df, file = "Funcs_lots_df/salva_df.RData")
