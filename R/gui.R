guiToolkit.default <- 'RGtk2'


get.available.indicators <- function() return(c('1m0', '5m0', '1q0', '5q0'))

modelLT.gui <- function() {
	require(gWidgets)
	quit.modelLT <- function(h, ...) {
		dispose(main.win)
	}
	options(guiToolkit=guiToolkit.default)
	e <- new.env()
	defaults <- formals(mod.lt)
	# main window
	main.win <- gwindow(paste('Model Life Tables v.',
			installed.packages()["LifeTables", "Version"]
			), 
			visible=TRUE, height=100, width=320, parent=c(400,150))
	main.g <- ggroup(horizontal=FALSE, cont=main.win)

	family.f <- gframe("<span color='blue'>Life Table Settings</span>", markup=TRUE, 
						horizontal=FALSE, cont=main.g)
	indicator.g <- glayout(cont=family.f)
	indicators <- get.available.indicators()
	indicator.g[1,1] <- ' '
	indicator.g[1,2, anchor=c(-1,0)] <- glabel('Indicator:', cont=indicator.g)
	indicator.g[1,3] <- e$ind.list <- gdroplist(indicators, cont=indicator.g, 
												selected=defaults$child.mort)
	indicator.g[1,4] <- glabel(' ', cont=indicator.g)
	indicator.g[1,5] <- e$ind.value <- gedit(width=10, cont=indicator.g)
	indicator.g[1,6] <- ' '
	
	indicator.g[2,2, anchor=c(-1,0)] <- glabel('Add adult mortality:', cont=indicator.g)
	indicator.g[2,3] <- e$adult.mort.chb <- gcheckbox('45q15', 
							checked=!is.null(defaults$adult.mort), cont=indicator.g,
							handler=function(h,...){enabled(e$admort.value) <- svalue(h$obj)})
	indicator.g[2,5] <- e$admort.value <- gedit(
							if (is.null(defaults$adult.mort)) '' else defaults$adult.mort, 
							width=10, cont=indicator.g)
	enabled(e$admort.value) <- svalue(e$adult.mort.chb)
	indicator.g[3,2, anchor=c(-1,0)] <- glabel('Sex:', cont=indicator.g)
	sex.values <- c('male', 'female')
	indicator.g[3,3] <- e$sex.list <- gdroplist(sex.values, cont=indicator.g,
										selected=which(sex.values==defaults$sex))
	
	addSpring(main.g)
	button.g <- ggroup(horizontal=TRUE, cont=main.g)
	gbutton('Quit', handler=quit.modelLT, cont=button.g) 
	addSpring(button.g)
	create.help.button(topic='modelLT.gui', package='LifeTables', parent.group=button.g,
                                                parent.window=main.win)
	#addSpace(button.g, 5)
	gbutton(action=gaction(label=' Generate Life Table ', icon='dataframe', handler=generateLT, 
				action=list(mw=main.win, env=e)), cont=button.g)
	
}

generateLT <- function(h, ...) {
	parent.e <- h$action$env
	e <- new.env()
	all.indicators <- get.available.indicators()
	ind.name <- svalue(parent.e$ind.list)
	indicator <- which(all.indicators == ind.name)
	ind.value <- as.numeric(svalue(parent.e$ind.value))
	sex <- svalue(parent.e$sex.list)
	use.adult.mort <- svalue(parent.e$adult.mort.chb)
	adult.mort.value <- svalue(parent.e$admort.value)
	life.table <- get.life.table(indicator, ind.value, e0=NULL, alpha=0, sex=sex, 
								add.adult.mort=use.adult.mort,
								adult.mort.value=adult.mort.value)

	e$label <- paste(ind.name, '=', ind.value)
	if (use.adult.mort) 
		e$label <- paste(e$label, ', 45q15 = ', adult.mort.value, sep='')
	e$label <- paste(e$label, ', family = ', life.table$family, ', ', sex, sep='')
	title <- paste('Life Table for', e$label)
	win <- gwindow(title, parent=h$action$mw, width=600, height=570)
	g <- ggroup(horizontal=FALSE, cont=win)
	par.g <- ggroup(horizontal=TRUE, cont=g)

	glabel('e0:', cont=par.g)
	e$e0 <- gedit(width=10, cont=par.g)
	addSpace(par.g, 10)
	glabel('alpha:', cont=par.g)
	e$alpha <- gedit(0, width=10, cont=par.g)
	glabel('   Changing values of e0 or alpha will update the life table.', cont=par.g)
	e$lt <- gtable(life.table$lt, cont=g, expand=TRUE)
	e$lt.object <- life.table
	svalue(e$e0) <- life.table$e0
	
	addHandlerChanged(e$e0, handler=updateLT, 
		action=list(indicator=indicator, ind.value=ind.value, what='e0', 
					sex=sex, add.adult.mort=use.adult.mort, 
					adult.mort.value=adult.mort.value, env=e))
	addHandlerChanged(e$alpha, handler=updateLT, 
		action=list(indicator=indicator, ind.value=ind.value, what='alpha', 
					sex=sex, add.adult.mort=use.adult.mort, 
					adult.mort.value=adult.mort.value, env=e))

	button.g <- ggroup(horizontal=TRUE, cont=g)
	gbutton('Cancel', handler=function(h, ...) dispose(win), cont=button.g)
	addSpring(button.g)
	plot.colnames <- setdiff(colnames(life.table$lt), c('Age', 'nax'))
	plot.defaults <- formals(plot.LifeTable)
	e$plot.column <- gdroplist(plot.colnames, 
						selected=which(plot.colnames == plot.defaults$lt.col), cont=button.g)
	e$log.scale <- gcheckbox('Log scale', checked=plot.defaults$log, cont=button.g, anchor=c(-1,0))
	# possible icons: 'lines'
	gbutton(action=gaction(label=' Plot ', icon='newplot', handler=plotLTcolumn, 
				action=list(mw=win, env=e)), cont=button.g)
	addSpace(button.g, 10)
	# posible icons: 'dataframe'
	gbutton(action=gaction(label=' Export ', icon='save', handler=exportLT, 
				action=list(mw=win, env=e)), cont=button.g)
}

get.life.table <- function(indicator, value, e0, alpha, sex, add.adult.mort=FALSE, 
								adult.mort.value=NULL) {
	adult.mort.value <- if(add.adult.mort) as.numeric(adult.mort.value)
	lt <- mod.lt(value, indicator, e0.target=e0, sex=sex, adult.mort=adult.mort.value, alpha=alpha)
	lt$lt <- data.frame(lt$lt)
	lt$lt[,'Age'] <- as.integer(lt$lt[,'Age'])
	return(lt)
}

updateLT <- function(h, ...) {
	e <- h$action$env
	what <- h$action$what
	if(what=='e0') {
		e0 <- as.numeric(svalue(e$e0))
		alpha <- NULL
		change <- 'alpha'
	} else {
		alpha.char <- svalue(e$alpha)
		alpha <- if (nchar(alpha.char) == 0) NULL else as.numeric(alpha.char)
		e0 <- NULL
		change <- 'e0'
	}
	lt <- get.life.table(h$action$indicator, h$action$ind.value, e0=e0, 
								alpha=alpha,
								sex=h$action$sex, 
								add.adult.mort=h$action$add.adult.mort,
								adult.mort.value=h$action$adult.mort.value)
	e$lt[,] <- lt$lt
	e$lt.object <- lt
	blockHandler(e[[change]])
	svalue(e[[change]]) <- round(lt[[change]], 5)
	unblockHandler(e[[change]])
}

exportLT <- function(h, ...) {
	e <- h$action$env
	filter <- list('csv files'=list(patterns='*.csv'), 'text files'=list(patterns='*.txt')) 
	gfile(type='save', quote=FALSE,
		filter=c(filter, list("All files" = list(patterns = c("*")))),
		handler=function(h1,...){
					filename <- h1$file
					pattern <- paste('[.]csv|txt', '$', sep='')
					if(length(grep(pattern, filename))==0)
						filename <- paste(filename, 'csv', sep='.') # add suffix if not present
					write.table(e$lt.object$lt, file=filename, sep=", ", row.names=FALSE)
			}
		)
}

plotLTcolumn <- function(h, ...) {
	e <- h$action$env
	lt <- e$lt.object
	#age <- as.numeric(lt[,'Age'])
	measure.name <- svalue(e$plot.column)
	#values <- as.numeric(lt[,measure.name])
	create.plot.window(e$mw, title='Life Table Plot')
	plot(lt, measure.name, log=svalue(e$log.scale))
	#l <- length(age) - 1
	#plot(age, values, main=paste(measure.name, 'for e0 =', svalue(e$e0), 'and', e$label), 
	#		ylab=measure.name, xlab='Age', type='b', panel.first = grid(l, l))

}

create.plot.window <- function(parent, title='', dpi=80, ps=10, ...) {
	e <- new.env()
	win <- gwindow(title, parent=parent, horizontal=FALSE)
	g <- ggroup(cont=win, horizontal=FALSE, expand=TRUE)
	g1 <- ggroup(cont=g, horizontal=TRUE)
	glabel("Output type:", cont=g1)
	e$type <- gdroplist(c("pdf", "postscript", "png", "jpeg", "tiff", "bmp"), cont=g1)
	gb <- gbutton('Save', cont=g1)
	g2 <- ggroup(cont=g, horizontal=TRUE, expand=TRUE)
	ggraphics(cont=g2, ps=ps, dpi=dpi, ...)
	addHandlerClicked(gb, handler=saveGraph, action=list(mw=win, env=e, dpi=dpi, dev=dev.cur()))
	Sys.sleep(1)
	return(g)
}

saveGraph <- function(h, ...){
	e <- h$action$env
	type <- svalue(e$type)
	postfix <- list(png='png', jpeg='jpg', pdf='pdf', tiff='tiff', bmp='bmp', postscript='ps')
	filter <- list()
	filter[[paste(type, 'files')]] <- list(patterns=paste('*', postfix[[type]], sep='.'))
	gfile(type='save', quote=FALSE,
		filter=c(filter, list("All files" = list(patterns = c("*")))),
		handler=function(h1,...){
						filename <- h1$file
						pattern <- paste('[.]', postfix[[type]], '$|[.]', type, '$', sep='')
						if(length(grep(pattern, filename))==0)
							filename <- paste(filename,  postfix[[type]], sep='.')
						size <- list()
						for(measure in c('width', 'height')) {
							size[[measure]]  <- NULL
							if(is.null(e[[measure]])) {
								if(type != 'pdf' && type != 'postscript') 
										size[[measure]] <- h$action$dpi*6
							} else {
								if(e[[measure]] != 'default') {
									size[[measure]] <- if(!is.null(e[[measure]][[type]])) e[[measure]][[type]]
											else size[[measure]] <- e[[measure]]
								}
							}
						}
						dev.set(h$action$dev)
						do.call('dev.print', c(list(file=filename, device=eval(parse(text=type))), size))
				}
		)
}

create.help.button <- function(topic, package, parent.group, parent.window) {
	helpaction <- gaction(label='Help', icon='help', 
                	handler=function(h, ...) {
                        oldhtmloption <- options(htmlhelp=FALSE);
                        oldchmoption <- options(chmhelp=FALSE);
                        ghelp(topic=topic, package=package, 
                                        cont=gwindow(parent=parent.window, width=700, height=700));
                        options(htmlhelp=oldhtmloption);
                        options(chmhelp=oldchmoption)
                })
	gbutton(action=helpaction, cont=parent.group)
}
