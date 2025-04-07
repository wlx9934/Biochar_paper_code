
#install.packages('devtools')
#devtools::install_github('gastonstat/plspm')
library(plspm)
library(vegan)

dat3 <- read.csv(file.choose(),header = T)
dat2 <- dat3[1:21]
dat <- decostand(dat2,method="standardize",MARGIN=2)



#Specify latent variables and store the relationship between variables and latent variables in R as a list： A = c('xxx','yyy')
dat_blocks <- list(
  A = 'A',
  B = 'B',
  C  =  c('X','Y','Z'),
  D  =  c('T','U','V','W'),
  E  =  c('O','P','Q','R'), 
  F  =  c('K','L','M'), 
  G  =  c('H','I','J')
)
dat_blocks

#Describe the correlation between latent variables through a 0-1 matrix, where 0 represents no correlation between variables and 1 represents correlation



A <- c(0, 0, 0, 0, 0, 0, 0)
B <- c(0, 0, 0, 0, 0, 0, 0)
C <- c(1, 1, 0, 0, 0, 0, 0)
D <- c(1, 1, 0, 0, 0, 0, 0)
E <- c(1, 1, 0, 0, 0, 0, 0)
F <- c(1, 1, 0, 0, 0, 0, 0)
G <- c(1, 1, 1, 1, 1, 1, 1)

dat_path <- rbind(A,B,C,D,E,F,G)
colnames(dat_path) <- rownames(dat_path)
dat_path

#Specify causal relationship, optional A (representing the cause of columns being rows) or B (representing the cause of rows being columns)
#If we transpose the 0-1 matrix or replace A with B, changing one of the two, the path arrows will be reversed
dat_modes <- rep('A', 7)
dat_modes

#modeling PLS-SEM
dat_pls <- plspm(dat, dat_path, dat_blocks, modes = dat_modes)
dat_pls
summary(dat_pls)

#View parameter estimates of path coefficients and related statistical information
dat_pls$path_coefs
dat_pls$inner_model


Paths = dat_pls$path_coefs
arrow_lwd = 10 * abs(round(Paths,2))

#View the path diagram of causal relationships
innerplot(dat_pls,arr.lwd = arrow_lwd, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'black', box.lwd = 0)
#View the status of external latent variables and internal latent variables
dat_pls$inner_summary
#?鿴??�#View the direct or indirect impact status between variables
dat_pls$effects
#?鿴?۲???�#To view the relationship between observed variables and latent variables, outerplot() can be used to draw a structure similar to a path diagram
dat_pls$outer_model
outerplot(dat_pls, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
#goodness-of-fit ֵ
dat_pls$gof
#View latent variable scores,
dat_pls$scores
#Output the value of the latent variable
latent <- data.frame(dat_pls$scores)
latent <- cbind(dat2$site, latent)
write.csv(latent, 'xxx.csv')

