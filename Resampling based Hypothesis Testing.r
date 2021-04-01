method1 = function(B1 = 99){
  R = 596
  alpha = 0.05
  delta.val = c(0, 0.5, 1, 1.5, 2)
  n1 = 8
  n2 = 4
  m = n1+n2
  mu = 0
  T = numeric(R)
  power = numeric(5)
  p.value =  numeric(R)
  T.stat = numeric(B1)
  cv = numeric(B1)
  So = matrix(0, nrow = (m), ncol = B1)
  for (d in 1:5){
    for (j in 1:R){
      S = c(rnorm(n1, mean = delta.val[d]), rnorm(n2, mean = mu))
      T[j] = t.test(S[1:n1], S[(n1+1):(m)], alternative = "greater", var.eq = TRUE)$statistic
      for (i in 1:B1){
        So[,i] = sample(S, size = (m), replace = FALSE)
        T.stat[i] = t.test(So[1:n1, i], So[(n1+1):m, i], alternative = "greater", var.eq = TRUE)$statistic
        cv[i]= quantile(T.stat[i], probs = (1-alpha))
      }
      p.value[j] = mean(as.integer(cv >= T[j]))
    }
    power[d] = mean(as.integer(p.value < alpha))
  }
  return(power)
}

method2 = function(B2, n =1){
  R = 596
  alpha = 0.05
  delta.val = c(0, 0.5, 1, 1.5, 2)
  n1 = 8
  n2 = 4
  m = n1+n2
  mu = 0
  p = matrix(0, nrow = n, ncol = 5)
  T = numeric(R)
  p.value = numeric(R)
  T.stat = matrix(0, nrow = B2, ncol = R)
  So = matrix(0, nrow = m, ncol = B2)
  for (k in 1:n){
    for (d in 1:5){
      for (j in 1:R){
        S = c(rnorm(n1, mean = delta.val[d]), rnorm(n2, mean = mu))
        T[j] = t.test(S[1:n1], S[(n1+1):m], alternative = "greater", var.eq = TRUE)$statistic
        for (i in 1:B2){
          So[,i] = sample(S, size = m, replace = FALSE)
          T.stat[i,j] = t.test(So[1:n1, i], So[(n1+1):m, i], alternative = "greater", var.eq = TRUE)$statistic
        }
      }
      cv= quantile(T.stat, probs = (1-alpha))
      for(j in 1:R){
        p.value[j] = mean(as.integer(cv >= T[j]))
      }
      p[k, d] = mean(as.integer(p.value < alpha))
    }
  }
  power = c(mean(p[,1]), mean(p[,2]), mean(p[,3]), mean(p[,4]), mean(p[,5]))
  return(power)
}

method3 = function(B3, n = 1){
  R = 596
  alpha = 0.05
  delta.val = c(0, 0.5, 1, 1.5, 2)
  n1 = 8
  n2 = 4
  m = n1+n2
  mu = 0
  p = matrix(0, nrow = n, ncol = 5)
  T =numeric(R) 
  p.value = numeric(R)
  T.stat = numeric(B3)
  So = matrix(0, nrow = m, ncol = B3)
  for (k in 1:n){
    for (d in 1:5){
      for (j in 1:R){
        S = c(rnorm(n1, mean = delta.val[d]), rnorm(n2, mean = mu))
        T[j] = t.test(S[1:n1], S[(n1+1):m], alternative = "greater", var.eq = TRUE)$statistic
      }
      for (i in 1:B3){
        So[,i] = sample(S, size = m, replace = TRUE)
        T.stat[i] = t.test(So[1:n1, i], So[(n1+1):m, i], alternative = "greater", var.eq = TRUE)$statistic
      }
     
      cv= quantile(T.stat, probs = 0.95)

      for (j in 1:R){
        p.value[j] = mean(as.integer(cv >= T[j]))
      }
      p[k, d] = mean(as.integer(p.value < alpha))
    }
  }
  power = c(mean(p[,1]), mean(p[,2]), mean(p[,3]), mean(p[,4]), mean(p[,5]))
  return(power)
}

Calc.RMSE = function(power){
  pow.del1 = sqrt(mean((power[,1]-0.05)^2))
  pow.del2 = sqrt(mean((power[,2]-0.189)^2))
  pow.del3 = sqrt(mean((power[,3]-0.451)^2))
  pow.del4 = sqrt(mean((power[,4]-0.737)^2))
  pow.del5 = sqrt(mean((power[,5]-0.918)^2))
  return(c(pow.del1, pow.del2, pow.del3, pow.del4, pow.del5))
}


Calc.Bias = function(power){
  bias.del1 = mean(power[,1]) - 0.05
  bias.del2 = mean(power[,2]) - 0.189
  bias.del3 = mean(power[,3]) - 0.451
  bias.del4 = mean(power[,4]) - 0.737
  bias.del5 = mean(power[,5]) - 0.918
  return(c(bias.del1, bias.del2, bias.del3, bias.del4, bias.del5))
}


delta = c(0, 0.5, 1, 1.5, 2)  
replicates = 25             
alpha = 0.05
RMSE.mat = matrix(0,9,5)
Bias.mat = matrix(0,9,5)
power.mat = matrix(0,replicates,5)

for(i in 1:replicates){
  power.mat[i,] = method1(B1 = 99) 
}
RMSE.mat[1, ] = Calc.RMSE(power.mat)
Bias.mat[1, ] = Calc.Bias(power.mat)


for(i in 1:replicates){
  power.mat[i,] = method2(B2 = 1)
}
RMSE.mat[2, ] = Calc.RMSE(power.mat)
Bias.mat[2, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method2(B2 = 2)
}
RMSE.mat[3, ] = Calc.RMSE(power.mat)
Bias.mat[3, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method2(B2 = 99)
}
RMSE.mat[4, ] = Calc.RMSE(power.mat)
Bias.mat[4, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method2(B2 = 1, n = 4)
}
RMSE.mat[5, ] = Calc.RMSE(power.mat)
Bias.mat[5, ] = Calc.Bias(power.mat)


for(i in 1:replicates){
  power.mat[i,] = method3(B3 = 596*1)
}
RMSE.mat[6, ] = Calc.RMSE(power.mat)
Bias.mat[6, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method3(B3 = 596*2)
}
RMSE.mat[7, ] = Calc.RMSE(power.mat)
Bias.mat[7, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method3(B3 = 596*99)
}
RMSE.mat[8, ] = Calc.RMSE(power.mat)
Bias.mat[8, ] = Calc.Bias(power.mat)

for(i in 1:replicates){
  power.mat[i,] = method3(B3 = 596*1, n = 4)
}
RMSE.mat[9, ] = Calc.RMSE(power.mat)
Bias.mat[9, ] = Calc.Bias(power.mat)

RMSE.table = data.frame(round(RMSE.mat, digits = 3))
names(RMSE.table) = c("Delta = 0", "Delta = 0.5", "Delta = 1.0", "Delta = 1.5", "Delta = 2.0")
row.names(RMSE.table) = c("Pow 1,99","Pow 2,1", "Pow 2,2","Pow 2,99","Pow (2,1)x4","Pow 3,1","Pow 3,2","Pow 3,99","Pow (3,1)x4")

Bias.table = data.frame(Bias.mat)
names(Bias.table) = c("Delta = 0", "Delta = 0.5", "Delta = 1.0", "Delta = 1.5", "Delta = 2.0")
row.names(Bias.table)=c("Pow 1,99","Pow 2,1", "Pow 2,2","Pow 2,99","Pow (2,1)x4","Pow 3,1","Pow 3,2","Pow 3,99","Pow (3,1)x4")


 print(RMSE.table)

print(Bias.table * 1000)


