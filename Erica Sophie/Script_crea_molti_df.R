## Loop per creare in sequenza i 50 dataset da confrontare 

# Librerie
library(bayesm)
library(MCMCpack)
library(mvtnorm)

load("function_build_X.RData")  # creare velocemente la matrice X.i
load("Funcs_lots_df/jittering.RData")    # jittering dei dati
load("Funcs_lots_df/build_list_X.i.RData")  # lista di X.i usata nel parallelo
load("Funcs_lots_df/funcs_for_parallel.RData") # funzioni per runnare in parallelo
load("Funcs_lots_df/coeff.RData") #funzione per crearei coefficienti
load("Funcs_lots_df/calcolo_W.i.RData")

# librerie per il calcolo in parallelo
library(pbapply)
library(parallel)

# detect che number of cores 
cl=makeCluster(detectCores())

# get the data
data(margarine)

# considero la covariata 'prezzo' di tutti i prodotti
product_specific <- margarine$choicePrice[,3:12]
head(product_specific)

p = dim(product_specific)[2]
n = dim(product_specific)[1]

ID_product_specific <- margarine$choicePrice$hhid

# degli agenti prendo in considerazione l'income, e le due dummy sulla dimensione del nucleo familiare
agent_specific <- margarine$demos[,2:4]

ID_agent_spec <- margarine$demos$hhid


# jittering (devo farlo per 50 volte) k indice della "volta"
for (k in 1:2){
  seed = k    #cambio il seed ad ogni iterazione
  
  new_cov = jittering(seed, agent_cov = agent_specific, product_cov = product_specific)
  new_agents_cov = new_cov[[1]]
  new_product_cov = new_cov[[2]]
  
  # parallelizing the building of X.i and of the list
  clusterExport(cl,varlist=list("ID_product_specific","ID_agent_spec",
                                "new_agents_cov", "new_product_cov" ,
                                "build_X", "build_list_of_X.i"))
  
  grid= 1:n
  
  list_Xi = pbsapply(grid, build_X.i_wrapper, cl=cl)
  
  # X.i dovrebbe avere dimensione p * 41
  # list_Xi ha dimensione 410 * n
  list.X.i <- list(0)
  
  # quindi devo wrappare ogni colonna della matrice list_Xi in una matrice 10 * 41
  for (i in 1:n){
    list.X.i[[i]] <- matrix(list_Xi[,i], nrow = p)
  }
  
  # calcolo i coefficienti per questo dataset
  
  BETA = coefficienti(seed, new_agents_cov, new_product_cov)
  
  W.i_and_Y = calcolo_W.i(seed, ID_product_specific, list.X.i, BETA)
  W.i.list = W.i_and_Y[[1]]
  Y = W.i_and_Y[[2]]
  hist(Y, main=paste(k,"iteration")) 
  
  crea_salva_df(n, p, k, list.X.i, W.i.list, Y, BETA)
  
}




