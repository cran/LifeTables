mortmod <-
function(pattern, sex="female", alpha=0){
		data(MLTobs)
		x <- seq(0,5,.5)
	 	csd.prop <- 1-pexp(x, rate=.75) 
 		f.csd.weight <- approxfun(x, csd.prop)
 		
 		w.ave <- function(csd, oad, csd.weight){
		oad.weight <- 1-csd.weight
		dev.out <- (csd*csd.weight) + (oad*oad.weight)
		return(dev.out)
			}

		if(alpha < 0){
			to.subtract <- w.ave(csd=lo.devs[pattern,], oad=lo.devs[6,], 
				csd.weight=f.csd.weight(abs(alpha)))
			model.patt <- averages.smooth[,pattern] + alpha*to.subtract} else {
			to.add <- w.ave(csd=hi.devs[pattern,], oad=hi.devs[6,], 
				csd.weight=f.csd.weight(abs(alpha)))
			model.patt <- averages.smooth[,pattern] + alpha*to.add
			}
			
			model.patt[model.patt>0] <- -.0001
			if(sex=="male"){
			return(structure(model.patt[1:24], class='MLT'))
				}
				
			if(sex=="female"){
			return(structure(model.patt[25:48], class='MLT'))
				}
		}

