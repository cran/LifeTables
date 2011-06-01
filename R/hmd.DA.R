hmd.DA <-
function(x, child.mort=4, sex="female", adult.mort=NULL){
	data(MLTobs)
	if(is.numeric(adult.mort)){
		x <- cbind(x, adult.mort)
		}

	if(sex=="male"){
		if(!is.numeric(adult.mort)){
			if(child.mort==1){mod.train <- hmd.1m0.train.m}
			if(child.mort==2){mod.train <- hmd.5m0.train.m}
			if(child.mort==3){mod.train <- hmd.1q0.train.m}
			if(child.mort==4){mod.train <- hmd.5q0.train.m}
			}
			
		if(is.numeric(adult.mort)){
			if(child.mort==1){mod.train <- hmd.1m0a.train.m}
			if(child.mort==2){mod.train <- hmd.5m0a.train.m}
			if(child.mort==3){mod.train <- hmd.1q0a.train.m}
			if(child.mort==4){mod.train <- hmd.5q0a.train.m}
			}
		}	
		
	if(sex=="female"){
		if(!is.numeric(adult.mort)){
			if(child.mort==1){mod.train <- hmd.1m0.train.f}
			if(child.mort==2){mod.train <- hmd.5m0.train.f}
			if(child.mort==3){mod.train <- hmd.1q0.train.f}
			if(child.mort==4){mod.train <- hmd.5q0.train.f}
			}
			
		if(is.numeric(adult.mort)){
			if(child.mort==1){mod.train <- hmd.1m0a.train.f}
			if(child.mort==2){mod.train <- hmd.5m0a.train.f}
			if(child.mort==3){mod.train <- hmd.1q0a.train.f}
			if(child.mort==4){mod.train <- hmd.5q0a.train.f}
			}
		}
		

	out.test <- mclustDAtest(data=x, models=mod.train)
	classification <- summary(out.test)$classification
	
	return(list(train=mod.train, test=out.test, classification=classification))
	}

