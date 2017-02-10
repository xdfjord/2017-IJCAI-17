score_fun = function (pre,real){
  if (nrow(pre)!=nrow(real) | ncol(pre)!=ncol(real))
    print ("Row number or Column number not match.")
  else {
    a=0
    for (i in 1:nrow(pre)) {
      for (j in 2:ncol(pre)){
        a=a+abs((pre[i,j]-real[i,j])/(pre[i,j]+real[i,j]))
      }
    }
    score=a/(nrow(pre)*(ncol(pre)-1))
    return(score)
  }
}

#test
a=matrix(c(1,2,3,5,3,2,6,4,5,7,6,4,8,3,9),nrow=3)
b=matrix(c(1,2,3,8,4,2,10,6,8,12,10,6,14,4,16),nrow=3)
score_fun(a,b)