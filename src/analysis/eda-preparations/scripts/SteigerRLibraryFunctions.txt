UnitVector<-function(n){matrix(rep(1,n),n,1)}
sympower <- function(x,pow) {
edecomp <- eigen(x)
roots <- edecomp$val
v <- edecomp$vec
d <- roots^pow
if(length(roots)==1) d <- matrix(d,1,1) else d <- diag(d)
sympow <- v %*% d %*% t(v)
sympow
}

P <-function(x) {
y <- x %*% solve(t(x) %*% x) %*% t(x)
y
}

Q <- function(x) {
q <- diag(dim(x)[1]) - P(x)
q
}

CompleteSymmetricMatrix <- function(x) {L <- length(x)
p <- (sqrt(8*L + 1) -1)/2
y <- matrix(0,p,p)
count <- 0
for(i in 1:p ) {for (j in 1:i){count <- count+1
y[i,j] <- x[count]
y[j,i] <- x[count]
}
}
y
}

CompleteCorrelationMatrix <- function(x) {L <- length(x)
p <- (sqrt(8*L + 1) +1)/2
y <- matrix(1,p,p)
count <- 0
for(i in 2:p ) {for (j in 1:(i-1)){count <- count+1
y[i,j] <- x[count]
y[j,i] <- x[count]
}
}
y
}

MakeExactData <- function(mean,var,n,use.population=FALSE) {
mu<-mean
sigma <- var
if(length(mu) == 1) mu <- matrix(mu,1,1) else if(!is.matrix(mu))mu <- matrix(mu,ncol=1)
if(length(sigma)==1) sigma <- matrix(sigma,1,1) else if(!is.matrix(sigma))sigma <- diag(sigma)
p <- dim(sigma)[1]
x <- matrix(rnorm(n*p),n,p)
input.cov <- if(use.population)((n-1)/n) * cov(x) else cov(x)
x <-x %*% sympower(input.cov,-1/2) %*% sympower(sigma,1/2)
diff <- matrix(0,1,p)
for(i in 1:p){
diff[1,i] <- mu[i]-mean(x[,i])
}
for(i in 1:n) {
  for(j in 1:p) {
    x[i,j] <- x[i,j] + diff[1,j]}
}
x
}


MultivariateNormalSample <-function(mu,sigma,n) {
p <- dim(sigma)[1]
x <- matrix(rnorm(n*p),n,p)
x <- x %*% sympower(sigma,1/2)
for(i in 1:n) {
  for(j in 1:p) {
    x[i,j] <- x[i,j] + mu[j]}
}
x
}

MakeFactorCorrelationMatrix <- function(F){
r <-F %*% t(F)
h <- diag(r)
u <- diag(1 - h)
r <- r+u
r
}

MakeFactorData <- function(F,n) {
r <-MakeFactorCorrelationMatrix(F)
mu<-matrix(0,1,dim(F)[1])
x <-MultivariateNormalSample(mu,r,n)
x
}

OrthogonalizeColumns <- function(x) {
x <- x %*% sympower(t(x) %*% x, -1/2)
x
}

OrthogonalRightUnit <- function(x) {
p <- dim(x)[1]
q <- dim(x)[2]
m <- q - p
decomp <-svd(x)
v <- decomp$u
d <- diag(decomp$d)
w1 <- decomp$v
w2 <- OrthogonalizeColumns(Q(w1) %*% matrix(rnorm(q*m),q,m))
S <- OrthogonalizeColumns( matrix(rnorm(m*m),m,m))
oru <- w1 %*% t(w1) + w2 %*% S %*% t(w2)
oru
}


MakeExactFactorData <- function(F, n){

r <-MakeFactorCorrelationMatrix(F)
mu<-matrix(0,1,dim(F)[1])
x <-MakeExactData(mu,r,n)
x
}

hat.matrix <- function(x,D,H)
{
m1 <- solve(t(D)%*%D)
m2 <- solve(t(H)%*%m1%*%H)
m3 <- t(x) %*% D %*% m1 %*% H
p <- dim(H)[2]
(1/p)*m3 %*% m2 %*% t(m3)
}

e.matrix <- function(x,D)
{
N <- dim(x)[1]
q <- dim(D)[2]
(t(x) %*% Q(D) %*% x )/(N-q)
}

H.matrix <- function(x,D,H)
{
m1 <- solve(t(D)%*%D)
m2 <- solve(t(H)%*%m1%*%H)
m3 <- t(x) %*% D %*% m1 %*% H
p <- dim(H)[2]
m3 %*% m2 %*% t(m3)
}

E.matrix <- function(x,D)
{
N <- dim(x)[1]
q <- dim(D)[2]
t(x) %*% Q(D) %*% x 
}

Raw.Discriminant.Weights <- function(x,D,H)
{
V <- Re(eigen(solve(E.matrix(x,D))%*% H.matrix(x,D,H))$vectors)
N <- dim(x)[1]
k <- dim(D)[2]
p <- dim(x)[2]
s <- min(k-1,p)
q <- dim(D)[2]
W <- e.matrix(x,D)
D <- diag(1/sqrt(diag(t(V) %*% W %*% V)))
A<-V %*% D
rownames(A) <- colnames(x)
return(A[,1:s])
}

Standardized.Discriminant.Weights <- function(x,D,H)
{
A <- Raw.Discriminant.Weights(x,D,H)
W <- e.matrix(x,D)
D.w <- diag(sqrt(diag(W)))
A.s <- D.w%*% A
k <- dim(D)[2]
p <- dim(x)[2]
s <- min(k-1,p)
rownames(A.s) <- colnames(x)
return(A.s[,1:s])
}

Discriminant.Eigenvalues <- function(x,D,H)
{
n <- min(dim(x)[2],dim(D)[2]-1)
zapsmall(Re(eigen(solve(E.matrix(x,D))%*% H.matrix(x,D,H))$values[1:n]))
}


Canonical.Table <- function(x,D,H)
{
k <- dim(D)[2]
p <- dim(x)[2]
N <- dim(x)[1]
s <- min(k-1,p)
w <- N-1-.5*(p+k)
F <- matrix(rep(0,s),s,1)
df1 <- matrix(rep(0,s),s,1)
df2 <- matrix(rep(0,s),s,1)
prob <- matrix(rep(0,s),s,1)
pct<- matrix(rep(0,s),s,1)
Lambda <- matrix(rep(0,s),s,1)
lam <- Discriminant.Eigenvalues(x,D,H)
sum.lam <- sum(lam)
cancorr <-matrix(rep(0,s),s,1)
for(m in 1:s)
{
num <- ((p-m+1)^2 * (k-m)^2 - 4)
if (num <= 0) t<-1 else 
t <- sqrt(
((p-m+1)^2 * (k-m)^2 - 4)/((p-m+1)^2+(k-m)^2-5))
df1[m] <- (p-m+1)*(k-m)
df2[m] <- w*t - .5*((p-m+1)*(k-m)-2)
Lambda[m] <- prod(1/(1+lam[m:s]))
pct[m] <- lam[m]/sum.lam
g <- Lambda[m]^(1/t)
F[m] <- (df2[m]/df1[m]) * (1-g)/(g)
prob[m] <- 1 - pf(F[m],df1[m],df2[m])
cancorr[m] <- sqrt(lam[m]/(1+lam[m]))
}
ss <- matrix(1:s,s,1)
lam <- matrix(lam,s,1)
data <- cbind(ss,lam,pct,cancorr,Lambda,F,df1,df2,prob)
colnames(data) <- c("Fcn", "Eigen","Prop","CanCorr","Lambda","F-Stat","df1","df2","prob")
return(round(data,4))
}



Make.D <- function(Grouping.Variable)
{
vecD <- as.matrix(unclass(factor(Grouping.Variable)))
ncol <- max(vecD)
nrow <- length(vecD)
D <- matrix(rep(0,nrow*ncol),nrow,ncol)
for(i in 1:nrow) D[i,vecD[i]]<-1
return(D)
}

Make.H <- function(Grouping.Variable)
{
vecD <- as.matrix(unclass(factor(Grouping.Variable)))
p <- max(vecD)
I <- diag(p-1)
rbind(I,-t(UnitVector(p-1)))
}

Raw.Discriminant.Intercepts <- function(x,D,H)
{
A <- Raw.Discriminant.Weights(x,D,H)
-colMeans(x %*% A)
}

Discriminant.Scores <- function(x,D,H)
{
A <- Raw.Discriminant.Weights(x,D,H)
if(!is.matrix(A)) A <- matrix(A,ncol=1)
b <- Raw.Discriminant.Intercepts(x,D,H)
one <- UnitVector(dim(x)[1])
x.aug <- cbind(x,one)
A.aug <- rbind(A,b)
return(x.aug %*% A.aug)
}

Plot.Discriminant.Scores <- function(x,D,H,Group,xmult=1,ymult=1)
{
d <- Discriminant.Scores(x,D,H)
if(dim(d)[2] ==1){
  x.complement <- Q(d) %*% x
  x.complement <- rowMeans(x.complement)
  data <- cbind(d,x.complement,Group)
ylabel="Complementary Dimension"}else {
data <- cbind(d[,1:2],Group)
ylabel="Discriminant Function 2"}
colnames(data) <- c("x","y","Group")
data <- data.frame(data)
data$x <- data$x * xmult
data$y <- data$y * ymult
scatterplot(y~x|Group,data,xlab="Discriminant Function 1",
ylab=ylabel,smooth=F,spread=F,boxplots=F,reg.line=F,main="Plot of Canonical Discriminant Scores")
pt <- Discriminant.Centroids(x,D,H,Group,xmult,ymult)
points(pt,pch=19,cex=2,col=palette())
}

Discriminant.Centroids <- function(x,D,H,Group,xmult=1,ymult=1)
{
d <- Discriminant.Scores(x,D,H)
if(dim(d)[2] ==1){
  x.complement <- Q(d) %*% x
  x.complement <- rowMeans(x.complement)
  d <- cbind(d,x.complement)}
aggdata <-aggregate(d, by=list(Group), 
  FUN=mean, na.rm=TRUE)
aggdata <- as.matrix(aggdata[,2:3])
aggdata <- aggdata %*% diag(c(xmult,ymult))
return(aggdata)
}

DiscriminantAnalysis <- function(x,Group){
D <- Make.D(Group)
H <- Make.H(Group)
##########################
## Canonical Discriminant Analysis Table
##########################
print("Canonical Table")
print(Canonical.Table(x,D,H))
#################################
## Coefficients
#################################
print("Standardized Discriminant Weights")
print(Standardized.Discriminant.Weights(x,D,H))
print("Raw Discriminant Weights")
print(Raw.Discriminant.Weights(x,D,H))
print("Raw Discriminant Intercepts")
print(Raw.Discriminant.Intercepts(x,D,H))

###############################################
##Plot Scores in Canonical Discriminant Space
###############################################

Plot.Discriminant.Scores(x,D,H,Group)

#################################################################
}



O <- function(n){
cbind(diag(n-1),-matrix(rep(1,n-1),n-1,1))
}

s <- function(n,k){
x<- matrix(rep(0,n),n,1)
x[k]<-1
return(x)
}

One <- function(n) {
matrix(rep(1,n),n,1)
}

### Courtesy of Keji Li!
vec_r=function(X){
    return(matrix(t(X),ncol=1))
}


