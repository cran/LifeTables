hmd.DA.mx <-
function(data, sex="female"){
	if(is.matrix(data)){
		age.groups <- ncol(data)
		} else age.groups <- length(data)
	
	if(sex=="male"){
		lt.m0.xp <- t(log(mlt.mx)[1:age.groups,])
		}	
		
	if(sex=="female"){
		lt.m0.xp <- t(log(flt.mx)[1:age.groups,])
		}
		
	hmd.m0.train <- mclustDAtrain(data=lt.m0.xp, labels=class5)
	hmd.m0.test <- mclustDAtest(data=data, models=hmd.m0.train)
	classification <- summary(hmd.m0.test)$classification
	
	return(list(train=hmd.m0.train, test=hmd.m0.test, classification=classification))
	
	}

