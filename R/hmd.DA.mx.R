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
		
	hmd.m0.train <- MclustDA(data=lt.m0.xp, class=class5)
	hmd.m0.test <- predict(hmd.m0.train, newdata=data)
	out.dens <- hmd.m0.test$z
	classification <- hmd.m0.test$classification
	
	return(list(train=hmd.m0.train, out.dens=out.dens, classification=classification))
	
	}

