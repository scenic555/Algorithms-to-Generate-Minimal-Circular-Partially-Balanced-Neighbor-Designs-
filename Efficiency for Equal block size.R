k=6 # p=k=No. of periods and b=n=No. of subjects=v*NB
v=28
y= 0
NB=2 #No. of sets of shifts

#Set of shifts  
T=c(3,4,5,6,8)#First set of shifts
T1=c(7,9,10,12,17) # Second and so on . . . .
#T2=c(13,14,15,20)
#T3=c(3,6,14)
#T4=c(10,13,15)
#T5=c(16,9,26)
#T6=c(22,6,32)
#T7=c(8,8,8)
#T8=c(9,9,9)
#T9=c(10,10,10)
#T10=c(11,11,11)
#T11=c(12,12,12)
#T12=c(13,13,13)
#T13=c(14,14,14)
#T14=c(15,15,15)
#T15=c(16,16,16)
#T16=c(17,17,17)
#T17=c(18,18,18)
#T18=c(19,19,19)
#T19=c(20,20,20)
#T20=c(21,21,21)
#T21=c(22,22,22)
#T22=c(23,23,23)
#Increase 

#Initial blocks from set of shifts
IB=c(y,cumsum(T)%%v)
IB1=c(y,cumsum(T1)%%v)
#IB2=c(y,cumsum(T2)%%v)
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
#IB18=c(y,cumsum(T18)%%v)
#IB19=c(y,cumsum(T19)%%v)
#IB20=c(y,cumsum(T20)%%v) 
#IB21=c(y,cumsum(T21)%%v)
#IB22=c(y,cumsum(T22)%%v)#Increase

#Initial blocks without set of shifts
#IB=c(4,2,3,1)   
#IB1=c(0,5,1)
#IB2=c(0,7,15,25)
#IB3=c(0,36,2,16)
#IB4=c(0,32,3,28)
#IB5=c(0,21,40,16)
#IB6=c(0,30,3,19)
p=seq(from=0, to=v-1, by=1)
w=rep(0,NB*v)

#print(ww3)
l=NULL;l1=NULL;l2=NULL;l3=NULL;l4=NULL;l5=NULL;l6=NULL;l7=NULL;l8=NULL;l9=NULL;l10=NULL;l11=NULL;l12=NULL;l13=NULL;l14=NULL;l15=NULL;l16=NULL;l17=NULL;l18=NULL;l19=NULL;l20=NULL;l21=NULL;l22=NULL

for(i in 1:k){
  for(j in 1:v){
    l=c(l,rep((IB[i]+p[j]+v)%% v))
    l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
    #l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
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
    #l17=c(l17,rep((IB17[i]+p[j]+v)%% v))
    #l18=c(l18,rep((IB18[i]+p[j]+v)%% v))
    #l19=c(l19,rep((IB19[i]+p[j]+v)%% v))
    #l20=c(l20,rep((IB20[i]+p[j]+v)%% v))
    #l21=c(l21,rep((IB21[i]+p[j]+v)%% v))
    #l22=c(l22,rep((IB22[i]+p[j]+v)%% v))
    #l23=c(l23,rep((IB23[i]+p[j]+v)%% v))#Increase
  }}

g= matrix(c(l),nrow=k,ncol=v,byrow =  TRUE)
g1= matrix(c(l1),nrow=k,ncol=v,byrow =  TRUE)
#g2= matrix(c(l2),nrow=k,ncol=v,byrow =  TRUE)
#g3= matrix(c(l3),nrow=k,ncol=v,byrow =  TRUE)
#g4= matrix(c(l4),nrow=k,ncol=v,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k,ncol=v,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k,ncol=v,byrow =  TRUE)
#g7= matrix(c(l7),nrow=k,ncol=v,byrow =  TRUE)
#g8= matrix(c(l8),nrow=k,ncol=v,byrow =  TRUE)
#g9= matrix(c(l9),nrow=k,ncol=v,byrow =  TRUE)
#g10= matrix(c(l10),nrow=k,ncol=v,byrow =  TRUE)
#g11= matrix(c(l11),nrow=k,ncol=v,byrow =  TRUE)
#g12= matrix(c(l12),nrow=k,ncol=v,byrow =  TRUE)
#g13= matrix(c(l13),nrow=k,ncol=v,byrow =  TRUE)
#g14= matrix(c(l14),nrow=k,ncol=v,byrow =  TRUE)
#g15= matrix(c(l15),nrow=k,ncol=v,byrow =  TRUE)
#g16= matrix(c(l16),nrow=k,ncol=v,byrow =  TRUE)
#g17= matrix(c(l17),nrow=k,ncol=v,byrow =  TRUE)
#g18= matrix(c(l18),nrow=k,ncol=v,byrow =  TRUE)
#g19= matrix(c(l19),nrow=k,ncol=v,byrow =  TRUE)
#g20= matrix(c(l20),nrow=k,ncol=v,byrow =  TRUE)
#g21= matrix(c(l21),nrow=k,ncol=v,byrow =  TRUE)
#g22= matrix(c(l22),nrow=k,ncol=v,byrow =  TRUE)#Increase 

G=cbind(g,g1)   #Increase as ,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22g1, g2,,g20,g21 . . .   
print(G)

#Model for repeated measurement designs  y=Pa+Ub+Tt+Ff
#P=Incidence matrix for period effect, U=incidence matrix for subject effect, T=for treatment effect 
#and F=for residual effect

P=kronecker(matrix(1,nrow=v*NB),diag(nrow = k))
print(P)
U=kronecker(diag(nrow = v*NB),matrix(1,nrow=k))
print(U)
wrapind <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G)
incmat <- matrix(0,ncol=v,nrow=prod(dim(G)),
                 dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G)))
  for (j in seq(nrow(G))) {
    if(j==1){
      tt <- table(as.character(G[wrapind(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G[wrapind(j-1,n),i]))
    }
    
    incmat[m,names(tt)] <- tt
    m <- m+1
  }
#for (i in seq(ncol(G)))
# for (j in seq(nrow(G))) {
#  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
# incmat[m,names(tt)] <- tt
#  m <- m+1
#}
wrap <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G)
incma <- matrix(0,ncol=v,nrow=prod(dim(G)),
                dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G)))
  for (j in seq(nrow(G))) {
    ttt<- table(as.character(G[wrap((j),n),i]))
    incma[u,names(ttt)] <- ttt
    u <- u+1
  }

s=t(incmat)%*%incma

E= as.data.frame(table(s[1,]))

a=apply(G, 2, function(z){
  ret <- numeric(v)
  for( ii in seq_along(z) )
  {
    ret[z[ii]+1] <- ret[z[ii]+1]+ 1}
  ret}) 

print("Treatment incidence matrix")
print(incma)
print("Carryover incidence matrix")
print(incmat)
N=t(incma)%*%U
print(N)
CM=(N)%*%t(N)
TT=t(incma)%*%incma
FF=t(incmat)%*%incmat
UU=t(U)%*%U
UI=solve(UU)
DP=t(incma)%*%P


M=t(incmat)%*%U

MM=M%*%t(M)

NM=N%*%t(M)




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

PI=((L-1/k*NM))
theta=t(incma)%*%incma-((1/k*CM))
er=eigen(theta)
ER=er$values
EER=ER[!ER%in%ER[v]]
EV=(v-1)/sum(1/EER)
Edirect=EV/CM[1,1]
si=t(incmat)%*%incmat-(1/k*MM)
ESI=eigen(si)
EESI=ESI$values
ESR=EESI[!EESI%in%EESI[v]]
ERR=(v-1)/sum(1/ESR)
Ecarryover=ERR/MM[1,1]


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
