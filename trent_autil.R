# Data Save
source('/gsa/yktgsa/home/e/m/emrah/ARFF/R/tutil.R')

mypaste<-function(t,post="") { paste(t,post,sep='') }
transformS<-function(x) {
	exp(-1*as.numeric(x)+1)
}
invtransformS<-function(x) {
	-1*(log(x)-1)
}
filterTRAIN<-function(TRAIN, isarap=NA, iscim=NA) {
	if (!is.na(isarap)) { if(isarap) { X<-subset(TRAIN, kosCins=="Arap") } else { X<-subset(TRAIN, kosCins!="Arap")}}
	if (!is.na(iscim)) { if(iscim) { X<-subset(X, kosPist=="Cim") } else { X<-subset(X, kosPist!="Cim")}}
	X<-subset(X, !is.na(S) & S>0)
	X<-subset(X, Derece>0)
	X$Hiz<-X$kosMesafe/X$Derece
	X<-subset(X, Hiz>0)
	X
}
filterXX<-function(TRAIN, isarap=NA,iscim=NA,isTRAIN=TRUE) {
	print(table(TRAIN$kosCins , TRAIN$kosPist ))
	if (isTRAIN) {
		X<-filterTRAIN(TRAIN,isarap,iscim)
	} else {
		X<-TRAIN
		if ( sum(X$S) ) {
			# another training matrix, derive Hiz
			X$Hiz<-X$kosMesafe/X$Derece
		} else {
			# X$Derece zero-vector already
			X$Hiz<-X$Derece
			X$S<-X$Derece
		}

	}
	# 3 Key metrics for evaluate a race S, Derece, and derived Hiz
	# universal check without Derece, our model is Hiz/S based
	# Regulate bad values (very hard numbers to beat)
	X<-subset(X, Gny<300)
	#	key Gny params have NaN (possibly div by 0)


	# Filter bad cols
#	X$Gny<-setNAN2Mean( X$Gny )
#	X$AtRecentS<-NULL
#	X$AtRecentS20<-NULL
#	X$AtRecentTabela<-NULL
#	X$AtRecentHC<-NULL


	# new features	
	X$kosMonth<-sapply( X$kosTarih, parseMonthFromTarih)

	# Factors available
	# AtRenk, AtCins
	# AtRecentYas numeric
	# kosSehir numeric
	# X$kosSehir<-factor(X$kosSehir,levels=1:8)
	
	featuresDeleted<-c("kosTarih", "kosSaat", "kosPist", "kosSira", 
	        	    "Yas", "Son6", "annUlke", "kosCins", "HK",
		            "AtId", "BabaId", "AnneId", "AntrenorId", "SahipId", "JokeyId", 
			    "AtRenk", "AtCins", 
			    "annIrk", "babIrk", "babUlke")

	# universal level for factors
	X$AtRenk=factor(as.character(X$AtRenk), levels=t(as.vector(TJKrenk[1,])))
	X$AtCins=factor(as.character(X$AtCins), levels=t(as.vector(TJKcins[1,])))
	levels(X$babIrk)<-c("Arap","Ingiliz", "Ingiliz")

	# factor variables for unique admittance
	# factorVariables<-c("AtRenk", "AtCins", "kosSehir")
	factorVariables=vector()

	# unique variables in the matrix - sans-transform
  	uniqueVariables<-c("AtRecentYas","kosMesafe")

	# Mesafe factors
	#  cx<-c(100, 1350, 1650, 1950, 2150, 10000)
	#  X$kosMesafeFactor <- cut(X$kosMesafe,breaks=cx)
  	
	# New/Modified features
	# relative ganyan'dan derive edilmis probability	

	isRemove<-grep("Adi",names(X))
	isRemove<-union(isRemove, grep("Name",names(X)))
	X<-X[,-isRemove]
	for (tmp in isRemove) X[, tmp]<-NULL
	for (tmp in featuresDeleted) X[, tmp]<-NULL

	# start transforms
	XX<-X
	Hiz<-X$Hiz
	S<-X$S
	Derece<-X$Derece

	# In Deriving bigger matrix, some variables will be removed for transforms
	# Remove key variables
	for (tmp in c("S", "Hiz", "Derece") ) XX[,tmp]<-NULL 
	# Remove factor variables
	for (tmp in factorVariables) XX[,tmp]<-NULL
	# Remove unique variables
	for (tmp in uniqueVariables) XX[,tmp]<-NULL

	# inverse
	IX<-1/XX
	colnames(IX)<-sapply(names(XX),mypaste,"inv")
	# square
	X2<-(XX^2)
	colnames(X2)<-as.vector(sapply(names(XX),mypaste,"sqr"))
	# log - only for positives
	XX2<-XX
	# filter is.nan, and positive features only. very bad way of doing thins
	#	selector<-vector()
	#for (tmp in 1:ncol(XX2)) {
	#	if (sort(XX2[,tmp],na.last=TRUE)[1]>0) { selector<-c(selector,tmp) }
	#}
	#XX2<-XX2[,selector]
	#LX<-log(XX2)
	#colnames(LX)<-as.vector(sapply(names(XX2),mypaste,"log"))
	# Log is given up, since some selected cols may appear in TRAIN but not in TEST
	
	# Super Duper Matrix
	XX=cbind(S,Hiz,Derece,XX,X2)
	# Append factor variables back to super matrix
	for (tmp in factorVariables) { eval(parse(text=paste(tmp,"<-X[,\"",tmp,"\"]",sep='')))}
	for (tmp in factorVariables) { eval(parse(text=paste("XX=cbind(XX,",tmp,")",sep=''))) }		
	# Append unique variables back to super matrix
	for (tmp in uniqueVariables) { eval(parse(text=paste(tmp,"<-X[,\"",tmp,"\"]",sep='')))}
	for (tmp in uniqueVariables) { eval(parse(text=paste("XX=cbind(XX,",tmp,")",sep=''))) }		

	if (1) {
		XX<-na.omit(XX); # takes care of NaN and NA's
		#XX<-XX[-which(apply(is.infinite(as.matrix(XX)) ,1,sum)>0),]
		#XX<-XX[,-which(is.na(cor(XX$Hiz,XX)))]

	}
	X<-XX
	# Transform S into exponential for GLM
	X$S<-transformS(X$S)
	X<-as.data.frame(X)
}

if (0) {
# All files together.
trainingFiles<-list.files("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/",pattern="train2")
load(mypaste("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/", trainingFiles[1]))
ALLTRAIN<-TRAIN[NULL,]
for (trainfile in trainingFiles) {
	print(paste("loading TRAIN data",trainfile))
	load(mypaste("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/", trainfile))
	ALLTRAIN<-rbind(ALLTRAIN,TRAIN)
}
trainfile="trainAll"
for (iscim in 0:1) {
	for (isarap in 0:1) {
		XX<-filterXX(ALLTRAIN,isarap,iscim)
		XX<-XX[ XX$S >= transformS(4.5),]
		print(ifelse(iscim,"Cim PIST","Kum PIST"))
		print(ifelse(isarap,"ARAP At", "INGILIZ At"))
		mfile=mypaste(trainfile,ifelse(iscim,".CIM.",".KUM."))
		mfile=mypaste(mfile,ifelse(isarap,"ARA.","ING."))
		XX=as.matrix(XX)
		write.table(file=mypaste(mfile,"in"),t(XX[,-(1:3)]),row.names=FALSE,col.names=FALSE)
		write.table(file=mypaste(mfile,"col"),XX[NULL,-(1:3)],row.names=FALSE,col.names=TRUE)
		for (k in 1:3) write.table(file=mypaste(mfile,colnames(XX)[k]),t(XX[,k]),row.names=FALSE,col.names=FALSE)
	}
}
}

if (0) {
# all DONE
for (trainfile in list.files("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/",pattern="train2")) {
	for (iscim in 0:1) {
		for (isarap in 0:1) {
			print(paste("loading TRAIN data",trainfile))
			load(mypaste("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/", trainfile))
			XX<-filterXX(TRAIN,isarap,iscim)
			XX<-XX[ XX$S >= transformS(4.5),]
			print(ifelse(iscim,"Cim PIST","Kum PIST"))
			print(ifelse(isarap,"ARAP At", "INGILIZ At"))
			mfile=mypaste(trainfile,".S1234.")
			mfile=mypaste(mfile,ifelse(iscim,".CIM.",".KUM."))
			mfile=mypaste(mfile,ifelse(isarap,"ARA.","ING."))
			XX=as.matrix(XX)
			write.table(file=mypaste(mfile,"in"),t(XX[,-(1:3)]),row.names=FALSE,col.names=FALSE)
			write.table(file=mypaste(mfile,"col"),XX[NULL,-(1:3)],row.names=FALSE,col.names=TRUE)
			for (k in 1:3) write.table(file=mypaste(mfile,colnames(XX)[k]),t(XX[,k]),row.names=FALSE,col.names=FALSE)
			}
	}
}
}

if (0) {
# all DONE
for (trainfile in list.files("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/",pattern="train2")) {
	for (iscim in 0:1) {
		for (isarap in 0:1) {
			print(paste("loading TRAIN data",trainfile))
			load(mypaste("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/", trainfile))
			XX<-filterXX(TRAIN,isarap,iscim)
			print(ifelse(iscim,"Cim PIST","Kum PIST"))
			print(ifelse(isarap,"ARAP At", "INGILIZ At"))
			mfile=mypaste(trainfile,ifelse(iscim,".CIM.",".KUM."))
			mfile=mypaste(mfile,ifelse(isarap,"ARA.","ING."))
			XX=as.matrix(XX)
			write.table(file=mypaste(mfile,"in"),t(XX[,-(1:3)]),row.names=FALSE,col.names=FALSE)
			write.table(file=mypaste(mfile,"col"),XX[NULL,-(1:3)],row.names=FALSE,col.names=TRUE)
			for (k in 1:3) write.table(file=mypaste(mfile,colnames(XX)[k]),t(XX[,k]),row.names=FALSE,col.names=FALSE)
			}
	}
}
}

if (0) {
for (progfile in list.files("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/",pattern="testprogTAG73568")) {
			print(paste("loading Program ",progfile))
			load(mypaste("/gsa/yktgsa/home/e/m/emrah/ARFF/R/veri/",progfile))
			XX<-filterXX(TEST,isTRAIN=FALSE)
			XX=as.matrix(XX)
			mfile=progfile
			write.table(file=mypaste(mfile,"in"),t(XX[,-(1:3)]),row.names=FALSE,col.names=FALSE)
			write.table(file=mypaste(mfile,"col"),XX[NULL,-(1:3)],row.names=FALSE,col.names=TRUE)
}
}


