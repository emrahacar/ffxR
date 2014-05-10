# for README
# get the test info and train to be used
text=read.table(file='README',stringsAsFactors=FALSE)
outputType=text[1,2]
train=text[2,2]
test=text[3,2]

# search ../run directories
# for proper pareto output, train and outputType
runs<-list.files('../', pattern='^run[1-9]')
for (r in runs) {
  text=read.table(file=paste('../',r,'/cmd', sep=''),stringsAsFactors=FALSE)
  trainfile=text[1,4]
  trainoutfile=text[1,5]
  if (length(grep(train,trainfile))) {
	if (length(grep(paste('.',outputType,sep=''),trainoutfile))) {
		break
	}
  }
}
# r is the train directory for pareto file		
testfile=paste('../', test,sep='')
testoutfile=gsub('[.]in',paste('.',outputType,sep=''),testfile)
colfile=gsub('[.]in','.col',testfile)
paretoFile=list.files(paste('../',r,sep=''),pattern='pareto',full.names=TRUE)[1]
R=read.table(file=paretoFile, sep=',', stringsAsFactors=FALSE,header=TRUE)
# pick last model only
R<-R[ nrow(R),]


print(trainfile)
print(testfile)
print(paretoFile)

X=read.table(file=testfile)
namesX=read.table(file=colfile)
y=t(read.table(file=testoutfile))

if (length(grep(".Derece", testoutfile))) isDecreasing=FALSE
if (length(grep(".S", testoutfile))) isDecreasing=TRUE
if (length(grep(".Hiz", testoutfile))) isDecreasing=TRUE

# function to be evaluated
# y<-S; isDecreasing=TRUE
# S=1 better than S=0.5
# y<-Derece; isDecreasing=FALSE
# y<-Hiz; isDecreasing=TRUE



# below is the evaluator for S, Hiz, Derece
X=as.data.frame(t(X))
names(X)<-t(namesX)
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
	wins<-0
	nb<-1
#	print(T)
	for(b in bet) {
	     if( tahmin[kname,"tahmin"]==nb)  wins<-T[T$sorder[1],"gny"]*b
	     nb<-nb+1
	}
  	tahmin[kname,"kazanc"]<-tahmin[kname,"kazanc"]+wins
	tahmin[kname,"gny"]<-T[T$sorder[1],"gny"]

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
for(modelno in rev(1:nrow(models))) {
	tahmin[[modelno]]<-senaryo(X, models[modelno,], isdecreasing=isDecreasing, tag=modelno)
	print(paste("ModelNO", modelno, " Kazanc ", sum(tahmin[[modelno]]$kazanc)))
	print(table(tahmin[[modelno]]$tahmin))

}
save(tahmin,file='tahmin.rda')

# load('tahmin.rda')
t=tahmin[[length(tahmin)]]
length(tahmin)
sum(t$kazanc)
sum(t$kazanc)/length(t$tahmin)
mean(t$tahmin)
table(t$tahmin)
table(cut(t$gny,breaks=1:20) , t$tahmin)
