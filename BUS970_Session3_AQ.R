############################################################
# BUS 970 | Spring 2024 | Session 3
# Anscombe's Quartet Class Exercise
# CK 
### REFERENCES
# https://en.wikipedia.org/wiki/Anscombe's_quartet
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
require(data.table)

#### LOAD DATA
## ,colClasses=c("numeric") forces fread to read the data in as numeric
# You'll run into trouble otherwise
aq1 <- fread("https://www.dropbox.com/scl/fi/ohh7aw5e1xzw2rpzaqoah/AQ_1.csv?rlkey=t4bg6hh0ijjc6jw0718h4py5b&dl=1")
aq2 <- fread("https://www.dropbox.com/scl/fi/2eq43exlx75y11o66x9o1/AQ_2.csv?rlkey=l4pqcnhawrcjq2qn9g005mdxp&dl=1")
aq3 <- fread("https://www.dropbox.com/scl/fi/3cf8hluzcqdazma0jlgp0/AQ_3.csv?rlkey=ef3fm56j5g900fsq6qr4vkymy&dl=1")
aq4 <- fread("https://www.dropbox.com/scl/fi/7mtudu1z4an1tkfusc97a/AQ_4.csv?rlkey=4x9i2hjum7zb1gha2g7l4blbd&dl=1")

# we perform repeated calculations - let's do a function
fn1 <- function(dt){
  x_mean <- mean(dt$x)
  y_mean <- mean(dt$y)
  x_var <- var(dt$x)
  y_var <- var(dt$y)
  corr_xy<-cor(dt$x,dt$y)
  ans <- cbind(x_mean,x_var,y_mean,y_var,corr_xy)
  return(ans)
}

fn1(aq1)
fn1(aq2)
fn1(aq3)
fn1(aq4)
    
# scatter plots
par(mfrow=c(2,2)) #  function to plot multiple plots on one page 
plot(aq1)
plot(aq2)
plot(aq3)
plot(aq4)

hist(aq1$y)
hist(aq2$y)
hist(aq3$y)
hist(aq4$y)
# Next week -  we see the effects of regressions / predictions if we don't account for different distributions

par(mfrow=c(1,1))
### TAKEAWAYS:
#1:  descriptive statistics is inadequate,
#2:  looking at a set of data graphically before starting to analyze 
# (next week) #3: type of relationships are important
















