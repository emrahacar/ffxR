# cmd is something like
# python2.7 ./runffx.py test ../train2007.rda.KUM.ING.in ../train2007.rda.KUM.ING.S ../train2007.rda.KUM.ING.in ../train2007.rda.KUM.ING.S ../train2007.rda.KUM.ING.col
text=read.table(file='cmd',stringsAsFactors=FALSE)
trainfile=text[1,4]
trainoutfile=text[1,5]
colfile=text[1,8]
print(trainfile)

# Hiz=t(read.table(file='train2008.rda.CIM.ARA.Hiz'))
# S=t(read.table(file='train2008.rda.CIM.ARA.S'))
# Derece=t(read.table(file='train2008.rda.CIM.ARA.Derece'))
X=read.table(file=trainfile)
namesX=read.table(file=colfile)
paretoFile=list.files(pattern='pareto')[1]
y=t(read.table(file=trainoutfile))

if (length(grep(".Derece", trainoutfile))) isDecreasing=FALSE
if (length(grep(".S", trainoutfile))) isDecreasing=TRUE
if (length(grep(".Hiz", trainoutfile))) isDecreasing=TRUE

# function to be evaluated
# y<-S; isDecreasing=TRUE
# S=1 better than S=0.5
# y<-Derece; isDecreasing=FALSE
# y<-Hiz; isDecreasing=TRUE



# below is the evaluator for S, Hiz, Derece
X=as.data.frame(t(X))
names(X)<-t(namesX)
R=read.table(file=paretoFile, sep=',', stringsAsFactors=FALSE,header=TRUE)
attach(X)
myeval<-function(x) {eval(parse(text=x))}
models<-lapply(R[,3], myeval)
models=as.data.frame(models)
models=t(models)
detach(X)
rownames(X)<-1:nrow(X)
rownames(y)<-1:nrow(y)
rownames(models)<-1:nrow(models)
######################################################
senaryo<-function(X, model, bet=10,isdecreasing=TRUE, tag="") {
  tahmin=data.frame()
  for (k in unique(X$kosId)) {
	K=subset(X,kosId==k)
	kname=as.character(k)
	i=as.numeric(rownames(K))
	T=data.frame( kosId=X$kosId[i], gny=X$Gny[i], est=model[i], actual=y[i], sorder=order(y[i],decreasing=isdecreasing), torder=order(model[i],decreasing=isdecreasing))
	tahmin[kname,"tahmin"] = which( T$torder==T$sorder[1])
	tahmin[kname,"kazanc"] = -sum(bet)
	tahmin[kname,"gny"]<-T[T$sorder[1],"gny"]
	wins<-0
	nb<-1
#	print(T)
	for(b in bet) {
	     if( tahmin[kname,"tahmin"]==nb)  wins<-T[T$sorder[1],"gny"]*b
	     nb<-nb+1
	}
  	tahmin[kname,"kazanc"]<-tahmin[kname,"kazanc"]+wins

	verbose=0
	if(verbose) {
	 	print(paste("= ", kname, " = kazanc =", wins-sum(bet), "== tag =", tag, "==", tahmin[kname,"tahmin"], " == Gny[1] was = ", T[T$sorder[1],"gny"], "========================"))
	}
	}	
	tahmin<-as.data.frame(tahmin)
}
#########################################################
#bet=c(5,3,2,1)
tahmin=list()
for(modelno in nrow(models)) {
	tahmin[[modelno]]<-senaryo(X, models[modelno,], isdecreasing=isDecreasing, tag=modelno)
	print(paste("ModelNO", modelno, " Kazanc ", sum(tahmin[[modelno]]$kazanc)))
	print(table(tahmin[[modelno]]$tahmin))

}
save(tahmin,file='tahmin.rda')