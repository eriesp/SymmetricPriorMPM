# MNP sui nostri dati
setwd("~/UNI/BAYESIAN STATISTICS/Project-GIT/SymmetricPriorMPM")

library(MNP)

X <- read.csv(file = "Datasets/Data_1/X.csv", header = TRUE, sep = ",")
X = X[1:1000,]
n = dim(X)[1]
p = 10

Y <- factor(X[seq(from = 1, to = n, by = p ),dim(X)[2]])
agent_cov = X[seq(from = 1, to = n, by = p ),12:14]


prod_cov <- data.frame(matrix(ncol = p, nrow = n/10))
colnames(prod_cov) = c("PPk_Stk", "PBB_Stk", "PFl_Stk", "PHse_Stk", "PGen_Stk", "PImp_Stk",
                       "PSS_Tub", "PPk_Tub", "PFl_Tub", "PHse_Tub")

for (i in 1:(n/10)){
  k=10*(i-1)
  prod_cov[i,] = t(X[(k+1):(k+10),42])
}

agent_prod = cbind(agent_cov,prod_cov)

accuracies_2 = numeric(p)

# con seed = 2 non vanno il 5, 7, 10
seed = 3 # ok gli altri tre

for (k in 8:p){

  set.seed(seed)
  res <- mnp(Y ~ 1 + income_t.1 + dummy_3_4.1 + dummy_5.1,
             data = agent_prod,
             cXnames = c("price"),
             choiceX=list('1' = PPk_Stk, '2'=PBB_Stk, '3'=PFl_Stk, '4'=PHse_Stk,'5'=PGen_Stk,
                          '6'=PImp_Stk,'7'=PSS_Tub, '8'=PPk_Tub,'9'=PFl_Tub, '10'=PHse_Tub),
             base = as.character(k) , n.draws = 2000,
             burnin = 500, thin = 5, verbose = TRUE)
  
  
  # summary(res)
  
  pred = predict(res, type = 'prob', n.draws = 1, verbose = TRUE)
  prob_pred = pred$p
  
  prob_pred = prob_pred[,c("1","2","3","4","5","6","7","8","9","10")]
  
  Y_pred = numeric(n/10)
  
  for (i in (1:(n/10))){
    Y_pred[i] = which(prob_pred[i,] == max(prob_pred[i,]))
  }
  
  # length(Y_pred)
  
  # table(Y,Y_pred)
  
  accuracies_2[k] = sum(Y_pred == Y)/(n/10)
  
}

accuracies_2


