k1=11
k2=9
# p=k=No. of periods and b=n=No. of subjects=v*NB
v=28
y= 0
#NB=2 #No. of sets of shifts
NB1=1 #No. of set of shifts for p1=k1 
NB2=2   ##No. of set of shifts for p2=k2

#Set of shifts  
T=c(1,2,3,4,5,6,7,8,9,11) #First set of shifts
T1=c(12,13,14,15,16,17,18,19) # Second and so on . . . .
T2=c(20,10,22,23,24,25,26,27)
#T3=c(17,21,24,26)
#T4=c(4,7,22,31)
#T5=c(36,37,38,46,41,42)
#T6=c(44,45,7,52,48,49)
#T7=c(40,43,53,54)
#T8=c(56,20,58,59)
#T9=c(3,13,33,34)
#T10=c(5,19,7)
#T11=c(17,8,11)
#T12=c(12,13,14)
#T13=c(4,26)
#T14=c(2,27)
#T15=c(21,35,40,41)
#T16=c(6,14,23,28)
#T17=c(15,32)
#T18=c(19,19,19)
#T19=c(20,20,20)
#T20=c(21,21,21)
#T21=c(22,22,22)
#T22=c(23,23,23)
#Increase 

#Initial blocks from set of shifts
IB=c(y,cumsum(T)%%v)
IB1=c(y,cumsum(T1)%%v)
IB2=c(y,cumsum(T2)%%v)
#IB3=c(y,cumsum(T3)%%v)
#IB4=c(y,cumsum(T4)%%v)
#IB5=c(y,cumsum(T5)%%v)
#IB6=c(y,cumsum(T6)%%v)
#IB7=c(y,cumsum(T7)%%v)
#IB8=c(y,cumsum(T8)%%v)
#IB9=c(y,cumsum(T9)%%v)
#IB10=c(y,cumsum(T10)%%v)
#IB11=c(y,cumsum(T11)%%v)
#IB12=c(y,cumsum(T12)%%v)
#IB13=c(y,cumsum(T13)%%v)
#IB14=c(y,cumsum(T14)%%v)
#IB15=c(y,cumsum(T15)%%v)
#IB16=c(y,cumsum(T16)%%v)
#IB17=c(y,cumsum(T17)%%v)
#Increase

#Initial blocks without set of shifts
#IB=c(0,1,3,2,4)   
#IB1=c(0,1,4)
#IB2=c(0,7,15,25)
#IB3=c(0,36,2,16)
#IB4=c(0,32,3,28)
#IB5=c(0,21,40,16)
#IB6=c(0,30,3,19)
p=seq(from=0, to=v-1, by=1)
w=rep(0,v)

#print(ww3)
l=NULL;l1=NULL;l2=NULL;l3=NULL;l4=NULL;l5=NULL;l6=NULL;l7=NULL;l8=NULL;l9=NULL;l10=NULL;l11=NULL;l12=NULL;l13=NULL;l14=NULL;l15=NULL;l16=NULL;l17=NULL;l18=NULL;l19=NULL;l20=NULL;l21=NULL;l22=NULL

# For Block size K1
for(i in 1:k1){
  for(j in 1:v){
    l=c(l,rep((IB[i]+p[j]+v)%% v))
   #l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
   # l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
    #l3=c(l3,rep((IB3[i]+p[j]+v)%% v))
    #l4=c(l4,rep((IB4[i]+p[j]+v)%% v))
     #l5=c(l5,rep((IB5[i]+p[j]+v)%% v))
    #l6=c(l6,rep((IB6[i]+p[j]+v)%% v))
    #l7=c(l7,rep((IB7[i]+p[j]+v)%% v))
    #l8=c(l8,rep((IB8[i]+p[j]+v)%% v))
    #l9=c(l9,rep((IB9[i]+p[j]+v)%% v))
    #l10=c(l10,rep((IB10[i]+p[j]+v)%% v))
    #l11=c(l11,rep((IB11[i]+p[j]+v)%% v))
    #l12=c(l12,rep((IB12[i]+p[j]+v)%% v))
    #l13=c(l13,rep((IB13[i]+p[j]+v)%% v))
    #l14=c(l14,rep((IB14[i]+p[j]+v)%% v))
    #l15=c(l15,rep((IB15[i]+p[j]+v)%% v))
    #l16=c(l16,rep((IB16[i]+p[j]+v)%% v))
    #l17=c(l11,rep((IB11[i]+p[j]+v)%% v))#Increase
  }
}


# For Block size K2
for(i in 1:k2)
  for(j in 1:v){
    #l=c(l,rep((IB[i]+p[j]+v)%% v))
      l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
     l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
     #l3=c(l3,rep((IB3[i]+p[j]+v)%% v))
# l4=c(l4,rep((IB4[i]+p[j]+v)%% v))
    #l5=c(l5,rep((IB5[i]+p[j]+v)%% v))
     #l6=c(l6,rep((IB6[i]+p[j]+v)%% v))
     #l7=c(l7,rep((IB7[i]+p[j]+v)%% v))
    #l8=c(l8,rep((IB8[i]+p[j]+v)%% v))
    #l9=c(l9,rep((IB9[i]+p[j]+v)%% v))
    #l10=c(l10,rep((IB10[i]+p[j]+v)%% v))
    #l11=c(l11,rep((IB11[i]+p[j]+v)%% v))
    #l12=c(l12,rep((IB12[i]+p[j]+v)%% v))
    #l13=c(l13,rep((IB13[i]+p[j]+v)%% v))
    #l14=c(l14,rep((IB14[i]+p[j]+v)%% v))
    #l15=c(l15,rep((IB15[i]+p[j]+v)%% v))
    #l16=c(l6,rep((IB16[i]+p[j]+v)%% v))
    #l17=c(l17,rep((IB17[i]+p[j]+v)%% v))#Increase
}

g = matrix(c(l),nrow=k1,ncol=v,byrow =  TRUE)
g1= matrix(c(l1),nrow=k2,ncol=v,byrow =  TRUE)
g2= matrix(c(l2),nrow=k2,ncol=v,byrow =  TRUE)
#g3= matrix(c(l3),nrow=k2,ncol=v,byrow =  TRUE)
#g4= matrix(c(l4),nrow=k2,ncol=v,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k1,ncol=v,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k1,ncol=v,byrow =  TRUE)
#g7= matrix(c(l7),nrow=k2,ncol=v,byrow =  TRUE)
#g8= matrix(c(l8),nrow=k2,ncol=v,byrow =  TRUE)
#g9= matrix(c(l9),nrow=k2,ncol=v,byrow =  TRUE)
#g10= matrix(c(l10),nrow=k1,ncol=v,byrow =  TRUE)
#g11= matrix(c(l11),nrow=k1,ncol=v,byrow =  TRUE)
#g12= matrix(c(l12),nrow=k1,ncol=v,byrow =  TRUE)
#g13= matrix(c(l13),nrow=k2,ncol=v,byrow =  TRUE)
#g14= matrix(c(l14),nrow=k2,ncol=v,byrow =  TRUE)
#g15= matrix(c(l15),nrow=k1,ncol=v,byrow =  TRUE)
#g16= matrix(c(l16),nrow=k1,ncol=v,byrow =  TRUE)
#g17= matrix(c(l17),nrow=k2,ncol=v,byrow =  TRUE)
#g12= matrix(c(l12),nrow=k,ncol=v,byrow =  TRUE)#Increase 

#G=cbind(g,g1)   #Increase as ,,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22g1, g2,,g20,g21 . . .   
#print(G)
#G1 Bind blocks of size k1
G1=cbind(g)   #Increase as g1, g2, . . .   
print(G1)
#G2 Bind blocks of size k2
G2=cbind(g1,g2)   #Increase as g1, g2, . . . 
print(G2)
#G3=cbind(g3)   #Increase as g1, g2, . . . 
#print(G2)
#G4=cbind(g3)   #Increase as g1, g2, . . . 
#print(G2)

#Model for repeated measurement designs  y=Pa+Ub+Tt+Ff
#P=Incidence matrix for period effect, U=incidence matrix for subject effect, T=for treatment effect 
#and F=for residual effect

P1=kronecker(matrix(1,nrow=v*NB1),diag(nrow = k1))
print(P1)
P2=kronecker(matrix(1,nrow=v*NB2),diag(nrow = k2))
print(P1)
U1=kronecker(diag(nrow = v*NB1),matrix(1,nrow=k1))
print(U1)
U2=kronecker(diag(nrow = v*NB2),matrix(1,nrow=k2))
print(U2)
wrapind <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G1)
incmat1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                 dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    if(j==1){
      tt <- table(as.character(G1[wrapind(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G1[wrapind(j-1,n),i]))
    }
    
    incmat1[m,names(tt)] <- tt
    m <- m+1
  }
wrapind1 <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G2)
incmat2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                  dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))) {
    if(j==1){
      tt <- table(as.character(G2[wrapind(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G2[wrapind(j-1,n),i]))
    }
    
    incmat2[m,names(tt)] <- tt
    m <- m+1
  }
incmat=rbind(incmat1,incmat2)
print(incmat)
#for (i in seq(ncol(G)))
 # for (j in seq(nrow(G))) {
  #  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
   # incmat[m,names(tt)] <- tt
  #  m <- m+1
  #}
wrap <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G1)
incma1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    ttt<- table(as.character(G1[wrap((j),n),i]))
    incma1[u,names(ttt)] <- ttt
    u <- u+1
  }
wrap1 <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G2)
incma2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))) {
    ttt<- table(as.character(G2[wrap((j),n),i]))
    incma2[u,names(ttt)] <- ttt
    u <- u+1
  }
incma=rbind(incma1,incma2)
s=t(incmat)%*%incma

E= as.data.frame(table(s[1,]))


a1=apply(G1, 2, function(z){
  ret <- numeric(v)
  for( ii in seq_along(z) )
  {
    ret[z[ii]+1] <- ret[z[ii]+1]+ 1}
  ret}) 
a2=apply(G2, 2, function(z){
  ret <- numeric(v)
  for( ii in seq_along(z) )
  {
    ret[z[ii]+1] <- ret[z[ii]+1]+ 1}
  ret}) 
A1=a1%*%t(a1)
A2=a2%*%t(a2)
a=cbind(a1,a2)
#print(a)
A=a%*%t(a)
print(A)

F= as.data.frame(table(A[1,]))
#print (E)
#print (F)
# for efficiency 
B11 = diag(A1)
B12 = diag(A2)
B13 = diag(B11)
B14 = diag(B12)
B1 = B13+B14
print("R matrix")
print(B1)
B2 = B1-(((1/k1)*A1)+((1/k2)*A2))
print(B2)
B3 = eigen(B2)
BB4 = B3$values
B4= round(BB4,digits = 4)
B5 = B4[!B4%in%B4[v]]
B6 =round( sum(1/B5),digits = 4)
B7= round((B6/(v-1)),digits = 4)
#print (IB)
#print (IB1)
#print (IB2)
#print (IB3)
#print (IB4)
print ("Efficiency Factor 'E'")
Efficiency= round((1/B7)*(1/A[1,1]),digits = 4)
print(Efficiency)
#print (B9)
#print("********************************************")
U_bound=round((((k1-1)*v)/((v-1)*k1)),digits = 4)
#print("Upper Bound =")
#print (M10)
m1=t(incmat1)%*%U1
m2=t(incmat2)%*%U2
M1=m1%*%t(m1)
M2=m2%*%t(m2)
Mmatrix=cbind(m1,m2)
#print(a)
M=Mmatrix%*%t(Mmatrix)
print(M)
M11 = diag(M1)
M12 = diag(M2)
M13 = diag(M11)
M14 = diag(M12)
M111 = M13+M14
M22 = M111-(((1/k1)*M1)+((1/k2)*M2))
print(M22)
M3 = eigen(M22)
MM4 = M3$values
M4= round(MM4,digits = 4)
M5 = M4[!M4%in%M4[v]]
M6 =round(sum(1/M5),digits = 4)
M7= round((M6/(v-1)),digits = 4)
EfficiencyM=round((1/M7)*(1/M[1,1]),digits = 4)
print(EfficiencyM)


n1=t(incma1)%*%U1
print(n1)
n2=t(incma2)%*%U2
print(n2)
CM=n1%*%t(n1)+(n2%*%t(n2))
MM=m1%*%t(m1)+(m2%*%t(m2))
n=(1/k1*(n1%*%t(n1)))+(1/k2*(n2%*%t(n2)))
L1=t(incma1)%*%incmat1
L2=t(incma2)%*%incmat2
L=t(incma)%*%incmat
NO=matrix(c(1),nrow=v,ncol=1)
CN=cbind(NO,L)
CHS=chisq.test(CN)
print(CHS)
SUME= sum(CN)
value=CHS$statistic
VC=sqrt(((value)/SUME)/(min(v-1,v)))
ES=(1-VC)
print(ES)





m=(1/k1*(m1%*%t(m1)))+(1/k2*(m2%*%t(m2)))
nm=(1/k1*(n1%*%t(m1)))+(1/k2*(n2%*%t(m2)))


theta=t(incma)%*%incma-n
er=eigen(theta)
ER=er$values
EER=ER[!ER%in%ER[v]]
EV=(v-1)/sum(1/EER)
Edirect=EV/CM[1,1]

PI=L-nm
si=t(incmat)%*%incmat-m
ESI=eigen(si)
EESI=ESI$values
ESR=EESI[!EESI%in%EESI[v]]
ERR=(v-1)/sum(1/ESR)
Ecarryover=ERR/MM[1,1]
TS=NULL;PS=NULL; SS=NULL; MP=NULL; K=NULL; z=c()
for ( i in 0:(v-1))
{
  x<-cos(2*pi*i/v)
  y<- sin(2*pi*i/v)
  z<-complex(real = x, imaginary = y)
  
  TS[i]=((z^0)*theta[1,1])+(z^1)*theta[1,2]+(z^2)*theta[1,3]+(z^3)*theta[1,4]+(z^4)*theta[1,5]+(z^6)*theta[1,7]+(z^7)*theta[1,8]+(z^8)*theta[1,9]+(z^9)*theta[1,10]+(z^10)*theta[1,11]
  PS[i]=(z^0)*PI[1,1]+(z^1)*PI[1,2]+(z^2)*PI[1,3]+(z^3)*PI[1,4]+(z^4)*PI[1,5]+(z^5)*PI[1,6]++(z^6)*PI[1,7]+(z^7)*PI[1,8]+(z^8)*PI[1,9]+(z^9)*PI[1,10]+(z^10)*PI[1,11]
  SS[i]=(z^0)*si[1,1]+(z^1)*si[1,2]+(z^2)*si[1,3]+(z^3)*si[1,4]+(z^4)*si[1,5]+(z^5)*si[1,6]+(z^6)*si[1,7]+(z^7)*si[1,8]+(z^8)*si[1,9]+(z^9)*si[1,10]+(z^10)*si[1,11]
  MP[i]=Mod(PS[i])
  K[i]=(TS[i]*SS[i])-(MP[i]^2)
  SSE0=1/v*sum(((z^-0*SS)/K))
  TSE0=1/v*sum(((z^-0*TS)/K))
  PSE0=-1/v*sum(((z^-0*PS)/K))
  alpha0=1/v*sum(((z^-0)*(TS^-1)))
}

print(SSE0)
print(TSE0)
print(PSE0)
print(alpha0)

effd=(v-1)/(v*((NB1*k1)+(NB2*k2))*SSE0)
print(effd)
effr=(v-1)/(v*((NB1*k1)+(NB2*k2))*TSE0)
print(effr)
effp=(v-1)/(v*((NB1*k1)+(NB2*k2))*(SSE0+(2*PSE0)+TSE0))
print(effp)
efft=(v-1)/(v*((NB1*k1)+(NB2*k2))*alpha0)
print(efft)
 ctl1= cbind(theta,PI)
 ctl2=cbind(t(PI),si)
ctl=rbind(ctl1,ctl2)
ECT=eigen(ctl)
ECV=ECT$values
ECR=ECV[!ECV%in%ECV[v]]
ERC=(v-1)/sum(1/ECR)





# Analysis of neighbor designs with approach 01

#library(pracma)
#mean_vect=t(rep(1,n))
#print(mean_vect)
#X_mat_mod.1=cbind(1,X1,X2,X3)
#info_mat_mod.1=t(X_mat_mod.1)%*%X_mat_mod.1
#info_mat_mod.1_rank=qr(info_mat_mod.1)
#hat_mat_mod.1=X_mat_mod.1%*%pinv(t(X_mat_mod.1)%*%X_mat_mod.1)%*%t(X_mat_mod.1)
#hat_mat_mod.1_rank=qr(hat_mat_mod.1)

#X_mat_mod.3=cbind(1,X1,X3)
#info_mat_mod.3=t(X_mat_mod.3)%*%X_mat_mod.3
#info_mat_mod.3_rank=qr(info_mat_mod.3)
#hat_mat_mod.3=X_mat_mod.3%*%pinv(t(X_mat_mod.3)%*%X_mat_mod.3)%*%t(X_mat_mod.3)
#hat_mat_mod.3_rank=qr(hat_mat_mod.3)

#X_mat_mod.4=cbind(1,X3)
#info_mat_mod.4=t(X_mat_mod.4)%*%X_mat_mod.4
#info_mat_mod.4_rank=qr(info_mat_mod.4)
#hat_mat_mod.4=X_mat_mod.4%*%pinv(t(X_mat_mod.4)%*%X_mat_mod.4)%*%t(X_mat_mod.4)
#hat_mat_mod.4_rank=qr(hat_mat_mod.4)

#X_mat_meanmod.=cbind(1)
#info_mat_meanmod.=t(X_mat_meanmod.)%*%X_mat_meanmod.
#info_mat_meanmod._rank=qr(info_mat_meanmod.)
#hat_mat_meanmod.=X_mat_meanmod.%*%pinv(t(X_mat_meanmod.)%*%X_mat_meanmod.)%*%t(X_mat_meanmod.)
#hat_mat_meanmod._rank=qr(hat_mat_meanmod.)


#Model with treatment and neighbor effects

# Concurrence matrix
#nnt_com = t(X1)%*%X3%*%t(X3)%*%X1
#print(nnt_com)
# M matrix
#m_mat = t(X2)%*%X2
#L_mat = t(X1)%*%X2
#n1_mat = t(X1)%*%X3
#n2_mat = t(X2)%*%X3
#n3_mat = t(X2)%*%X2-t(X2)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X2
# Block incidence matrix
#b_com = t(X3)%*%X3
##Joint information matrix for direct treatments and neighbor effects
#c11=t(X1)%*%X1-t(X1)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X1
#c12=t(X1)%*%X2-t(X1)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X2
#c21=t(X2)%*%X1-t(X2)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X1
#c22=t(X2)%*%X2-t(X2)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X2
# Information matrix for direct treatments in the presence of neighbor effects
#info_com=c11-c12%*%(pinv(c22)%*%c21
#info_com_rank=qr(info_com)

#e_values= eigen(info_com)$values
#e_values_com=e_values[!e_values%in%e_values[v]]
 # e_values_Sum=sum(e_values_com)   # Sum of eign values
  #e_values_pro=1
   # for(i in 1: length(e_values_com)){
    #    pro=e_values_pro*e_values_com[i]
     #   e_values_pro=pro
      #    }
  #e_values_product=e_values_pro # product of eign values

# Information matrix for direct treatments wiuthout neighbor effects

#c111=t(X1)%*%X1-t(X1)%*%X3%*%(solve(t(X3)%*%X3))%*%t(X3)%*%X1
#e_values_tr= eigen(c11)
#X_neighbor=cbind(1,X1,X2,X3)
#X_not_neighbor=cbind(1,X1,X3)
#write.table(X1, file="E:R-output/X1.xlsx",append = FALSE, quote = FALSE,sep=" ", 
          # eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
          #  fileEncoding = "")
#write.table(X2, file="E:R-output/X2.txt",append = FALSE, quote = FALSE,sep=" ", 
           # eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
            #fileEncoding = "")
#write.table(X_not_neighbor, file="E:R-output/X3.txt",append = FALSE, quote = FALSE,sep=" ", 
          #  eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
          #  fileEncoding = "")
#write.table(X_neighbor, file="E:R-output/Combined.txt",append = FALSE, quote = FALSE,sep=" ", 
         #   eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
          #  fileEncoding = "")
#combined=round(c(Efficiency,U_bound,(Efficiency/U_bound)*100,e_values_Sum,e_values_product),digits = 4)
#print(combined)

#write.table(X1, file="E:R-output/X1.txt",append = FALSE, quote = FALSE,sep=" ", 
 #eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
 # fileEncoding = "")
#write.table(X2, file="E:R-output/X2.txt",append = FALSE, quote = FALSE,sep=" ", 
 #eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
  #fileEncoding = "")
#write.table(X3, file="E:R-output/X3.txt",append = FALSE, quote = FALSE,sep=" ", 
 #eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
 # fileEncoding = "")
#write.table(G, file="E:R-output/G.txt",append = FALSE, quote = FALSE,sep=" ", eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
 #           fileEncoding = "")
#write.table(nnt_com, file="E:R-output/nnt.txt",append = FALSE, quote = FALSE,sep=" ", eol="\n", na= "NA", dec=".",row.names = FALSE,col.names = FALSE,qmethod = c("escape", "double"),
 #           fileEncoding = "")
#combined=round(c(Efficiency,U_bound,(Efficiency/U_bound)*100),digits = 4)
#print(combined)
#F= as.data.frame(table(A[1,]))
#print (F)
