library(ggplot2)
D = function(n){
  Mean <- c()
  for(i in 1:10000){
    X = rexp(n, rate = 1)
    Mean[i] = round(mean(X), digits=3)
  }
  df=as.data.frame(table(Mean))
  df$Prob = df$Freq/10000
  return(df)
}
out = D(n=15) 
ggplot(out,aes(x=Mean,y=Prob))+geom_segment(aes(x=Mean,xend=Mean,y=0,yend=Prob),size=1)+ scale_x_discrete(breaks = seq(0,9))+theme_bw()
