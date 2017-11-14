downtime=c(0, 1, 2, 12, 12, 14, 18, 21, 21, 23, 24, 25, 28, 29, 30, 30, 30, 33, 36, 44, 45, 47,51)
mean(downtime)
median(downtime)
min(downtime)
max(downtime)
range(downtime)

sd(downtime)
quantile(downtime,0.05)
quantile(downtime,0.95)

table(downtime)
max(table(downtime))

a=rep(seq(0,4,1),5)
sort(a,decreasing = FALSE)

b=rep(seq(1,5,1),5)
b


A=c(61,175,111,124,13,21,24,23,4,18,14,18)
Q=matrix(A,nrow=4,dimnames=list(c("","","",""),c("x","y","z")))
Q[1,3]

sum_a=function(n){
  result = 0
  for(i in 1:n){
    result = result + (i/n)
  }
  print(result)
}

sum_b=function(n){
  result = log(n)+0.6
  print(result)
}

compare=function(n){
  sum_a(n)>sum_b(n)
}

compare(500)


x=0
tolerance=0.000001
count=0
repeat{
  f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
  if(abs(f) < tolerance) break
  f.prime=7*x^6+60000*x^5+5.3*x^4+42400*x^3+0.1815*x^2+1210*x+0.0005
  x=x-f/f.prime
  count=count+1
}
print(x)


attach(results)
x11(width=12,height=5)
par(mar=c(3,4,1,1))
par(mfrow=c(2,2))

boxplot(arch1~gender,xlab="gender",main="Architecture Semester 1")
boxplot(arch2~gender,xlab="gender",main="Architecture Semester 2")
boxplot(prog1~gender,xlab="gender",main="Porgramming Semester 1")
boxplot(prog2~gender,xlab="gender",main="Porgramming Semester 2")

factorial(4)
factorial(50)
help(lfactorial)

lfactorial(5000)

  result = 0
  for(i in 1:5000){
    log(i)
    result = log(i) +result
  }
  
  print(result)



help(log)

choose(4,2)
choose(50,20)
lchoose(5000,2000)


Filter(function(i) { all(i %% c(2,3,7) != 0) }, seq(100))

k = diag(c(1,1,1,1,1,1,1,1,1,1))
kk = apply(k,1,rev)
kkk=k+kk

help(replace)
replace(kkk,which(kkk==1),5)

k_5 = diag(c(5,5,5,5,5,5,5,5,5,5))
kk_5 = apply(K_5,1,rev)
kkk_5 = k_5 + kk_5

help(t)
a=matrix(1:30,5,6)
ta=t(a)
t(k)
kk = matrix(5,ncol = 10,nrow = 10)
diag(kk) = c(1,1,1,1,1,1,1,1,1,1)


prime = function(x){
  for (i in 2:x ){
    if(i %% 2 != 0 && i %% 3 != 0 && i %% 5 != 0 && i%% 7 !=0){
      print(i)
    }
  }
}

prime(10)


plot_aaa = function(x){
  if(x<=0){
    n = -(x^3)
  }
  else if(x>0 && x<=1){
    n= x^2
  }
  else{
    n = sqrt(x)
  }
}

plot_aaa(-2)

y=function(x){ 
  y=rep(0,1000)
  if(x<=0){
    x=x^3
    return(x) 
  }
  else if(x>=0&x<=1){
    x=x^2
    return(x) 
  }
  else{ 
    x=sqrt(x)
    return(x)
  }
}

x=seq(-2,2,0.1)
y(x)




help(plot)
table(downtime)

lchoose(5000,2000)