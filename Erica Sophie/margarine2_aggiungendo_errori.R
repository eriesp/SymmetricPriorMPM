
setwd("~/UNI/BAYESIAN STATISTICS/PTOJECT")
library(bayesm)
library(MCMCpack)
library(mvtnorm)

load("function_build_X.RData")

# MARGARINE DATASET
data(margarine)

# considero tutti i prodotti
product_specific <- margarine$choicePrice[,3:12]
head(product_specific)

p = dim(product_specific)[2]
n = dim(product_specific)[1]

ID_product_specific <- margarine$choicePrice$hhid


# degli agenti prendo in considerazione l'income, e le due dummy sulla dimensione del nucleo familiare
agent_specific <- margarine$demos[,2:4]
summary(agent_specific$Income)
head(agent_specific)
ID_agent_spec <- margarine$demos$hhid


# estraggo da -10 a 10 e poi aggiungo il minimo
agent_specific <- margarine$demos[,2:4]

eps = runif(dim(agent_specific)[1], -10, 10)

agent_specific$Income = agent_specific$Income + eps
summary(agent_specific$Income)

agent_specific$Income[which(agent_specific$Income <= 0)] = - agent_specific$Income[which(agent_specific$Income <= 0)]




# scalo la colonna degli income come abbiamo fatto precedentemente
income = agent_specific$Income

log_inc_1 = log(income + 6)
income_scaled_log_1 <-  2* (log_inc_1 - min(log_inc_1)) / (max(log_inc_1) - min(log_inc_1)) -1
summary(income_scaled_log_1)

plot(income_scaled_log_1)
abline(h= mean(income_scaled_log_1), col = 'red')

agent_specific$Income = income_scaled_log_1

n_a = dim(agent_specific)[1]




# jittering dummy dimensione famiglia 

dim_fam = sample(c(1,2,3), n_a, prob = c(0.45,0.45,0.1), replace = TRUE)

dummy3_4 = ifelse(dim_fam == 2, 1,0)
dummy5 = ifelse(dim_fam == 3, 1,0)

agent_specific$Fs3_4 = dummy3_4
agent_specific$Fs5. = dummy5



# jittering del prezzo


for (i in 1:p){
  eps = runif(n, -.1, .1)
  product_specific[,i] = product_specific[,i] + eps
}

summary(product_specific)



# list of matrices X_i
list.X.i <- list(0)
for (i in 1:n){
  curr_id <- ID_product_specific[i]
  X.i <- build_X(agent_specific[which(ID_agent_spec == curr_id),],
                 t(product_specific[i,]))
  list.X.i[[i]] <- X.i
}



#### coefficienti ####
mean_price <- colMeans(product_specific)

# intercetta
set.seed(12)
error <- rnorm(p, 0, 0.1)
beta_0 = mean_price - mean(mean_price) + error
beta_0
cor(mean_price, beta_0)

# coefficienti product specific = delta
delta <- runif(1,-1.25,-0.75)
delta

# csi
kd = 3

# csi deve essere lungo p*kd
p*kd

# csi è diviso in triplette, di cui il primo elmento si riferisce all'income,
# il secondo alla dummy sulla famiglia composta da 3 a 4 persone
# il terzo alla dummy sulla famiglia composta da 5 perone o più
set.seed(4)

# income
csi1 = 5 + c(- 1, -.7, .075, -2, -3, 0.05, 0.065, .075, .1, -.7)
csi1
cor(mean_price,csi1)

# fam con 3 o 4 componenti
csi2 = runif(p,0,1)

# fam con 5 o più componenti
csi3 = runif(p,0,1)

csi = numeric(p*kd)

for (i in 1:p){
  csi[((i-1)*kd +1):(i*kd)] = c(csi1[i],csi2[i],csi3[i])
}


#### beta grosso più grosso ####
BETA = c(beta_0,csi,delta)



### salvando tutte le scelte ####
choices = numeric(n)

identity = diag(p)
S = identity*(50-p-1)

sigma <- riwish(50,S)

for (i in 1:n){
  curr_id <- ID_product_specific[i]
  
  X.i_log = list.X.i[[i]]
  
  # set.seed(1)
  W.i_log = rmvnorm(1, X.i_log %*% BETA, sigma)
  W.i_log
  
  choices[i] = which(W.i_log == max(W.i_log))
}

hist(choices)


#### Creating the dataset ####

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


dim(X.i_df)

X.i_df= cbind(index,X.i_df)


Y = numeric(n*p)

for (i in 1:n){
  Y[((i*p)-p+1):(i*p)] = rep(choices[i],p)
}

X.i_df= cbind(X.i_df,Y)

beta_df = as.data.frame(BETA)

write.csv(X.i_df,"dataset_ripetuti/X.1.csv", row.names = FALSE)
write.csv(beta_df,"dataset_ripetuti/beta.1.csv", row.names = FALSE)

