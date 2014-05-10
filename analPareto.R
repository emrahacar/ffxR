
load('tahmin.rda')
t=tahmin[[length(tahmin)]]
length(tahmin)
sum(t$kazanc)
sum(t$kazanc)/length(t$tahmin)
mean(t$tahmin)
table(t$tahmin)
table(cut(t$gny,breaks=1:20) , t$tahmin)
