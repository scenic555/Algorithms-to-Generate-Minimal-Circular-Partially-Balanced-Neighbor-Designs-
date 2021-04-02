##########################################################################################
# CPBND_equalsize: Circular partially balance neighbour design for block of equal size(K)
# Algorithm from paper:
# Muhammad Nadeem, Khadija Noreen, Mahmood Ul Hassan, M. H. Tahir, Muhammad Rasheed and 
# Rashid Ahmed (2021). Algorithms to Generate Minimal Circular Partially Balanced Neighbor 
# Designs. 
# Code by Mahmood Ul Hassan and Rashid Ahmed, 2020-2021 
# Version 1.3.0  (2021-03-26)
##########################################################################################




################################################################
# Divion of adjusted A in i groups to get the set(s) of shifs
################################################################
grouping1<-function(A,k,v,i){
  bs<-c()
  z=0;f=1
  A1=A
  while(f<=i){
    
    for(y in 1:2000){
      com<-sample(A1,k)
      cs<-sum(com)
      if(cs%%v==0){
        bs<-rbind(bs,com)
        A1<-A1[!A1 %in% com]
        z<-z+1
        f=f+1
      }
      if(z==i) break
    }
    if(z<i) {bs<-c();z=0;f=1;A1=A}  
    
  }
  
 
  bs1<-t(apply(bs,1,sort))
  bs1<-cbind(bs1,rowSums(bs),rowSums(bs)/v)
  rownames(bs1)<-paste("G",1:i, sep="")
  colnames(bs1)<-c(paste(1:k, sep=""),"sum" ,"sum/v")
  
  bs2<-t(apply(bs,1,sort))
  bs2<-delmin(bs2)
  list(B1=bs2,B2=bs1,B3=A1)
  }


#######################################################################
# Obtaing set(s) of shifts by deleting smallest value of each group
#######################################################################

delmin<-function(z){
  fs<-c()
  n<-nrow(z)
  c<-ncol(z)-1
  for(i in 1:n){
    z1<-z[i,]
    z2<-z1[z1!=min(z1)]
    fs<-rbind(fs,z2)
  }
  rownames(fs)<-paste("S",1:n, sep="")
  colnames(fs)<-rep("",c)
  return(fs)
}


####################################################################################
# Selection of adjusted A and the set(s) of shifs to obtain Circular partially 
# balance neighbour design for block of equal size. 
##################################################################################

# D=1: minimal CPBNDs in which v/2 unordered pairs do not appear
# D=2: minimal CPBNDs in which 3v/2 unordered pairs do not appear 
#   K: Block sizes
#   i: Number of set of shifts for K


CPBND_equalsize<-function(k,i,D=1){
  
if(k<=3) stop("k= Block size: Block size must be greater than 3")
if(i<=0) stop("i= Must be a positive integer")

setClass( "stat_test", representation("list"))
  
setMethod("show", "stat_test", function(object) {
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
cat("Following are required sets of shifts to obtain the 
minimal CPBND for", "v=" ,object[[3]][1], "and","k=",object[[3]][2], "\n")
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    print(object[[1]])
  })

if(D==1){  

v=2*i*k+2; m=(v-2)/2

if(m%%4==0){
   A=1:m
   A1<-grouping1(A,k,v,i)
   A2<-c(v,k);names(A2)<-c("V","K")
   x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
}

if(m%%4==3){
  A<-c(1:((3*m-1)/4),((3*m+7)/4),((3*m+11)/4):m, (5*(m+1)/4))
  A1<-grouping1(A,k,v,i)
  A2<-c(v,k);names(A2)<-c("V","K")
  x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
 }

if(m%%4==1 |  m%%4==2){return("The minimal CPBNDs in which v/2 unordered pairs cannot be constructed for v=2ik+2 and k=block size")}
}

if(D==2){
v=2*i*k+4; m=(v-2)/2


if(m%%4==0){
  A=c(1:(m/2),((m+4)/2),((m+6)/2):(m-1),((3*m+2)/2))
  A1<-grouping1(A,k,v,i)
  A2<-c(v,k);names(A2)<-c("V","K")
  x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
}

if(m%%4==1){
  A=c(1:((3*m+1)/4),((3*m+9)/4),((3*m+13)/4):(m-1),((5*m+3)/4))
  A1<-grouping1(A,k,v,i)
  A2<-c(v,k);names(A2)<-c("V","K")
  x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
}

if(m%%4==2){
  A=c(2:(m-2),m,(2*m+1))
  A1<-grouping1(A,k,v,i)
  A2<-c(v,k);names(A2)<-c("V","K")
  x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
}

if(m%%4==3){
  A=c(1:((m+1)/4),((m+9)/4),((m+13)/4):(m-2), m, ((7*m+3)/4))
  A1<-grouping1(A,k,v,i)
  A2<-c(v,k);names(A2)<-c("V","K")
  x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
}

}
new("stat_test", x)

}


##################################################################################
# Examples: Using CPBND_equalsize function to obtain the set(s) of shifs
# for construction of circular partially balance neighbour design for equal block  
# sizes (k1)
##################################################################################



# example#1
CPBND_equalsize(k=12,i=6,D=1)


# example #2
CPBND_equalsize(k=11,i=5,D=2)




