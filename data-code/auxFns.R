###############################################
# Auxiliary functions
#
# Marcelo Ponce
###############################################

# customized print function
mprint <- function(mstr, hdr=paste0(rep("#",nchar(mstr)+2),collapse='') ) {
	
	if (!is.na(hdr)) cat(paste(hdr,"\n"))
	cat(paste(mstr,'\n'))
	if (!is.na(hdr)) cat(paste(hdr,"\n"))
}

###############################################

pause <- function(msg="Press [enter] to continue...") {

	invisible(readline(prompt=msg))

}
###############################################

savePlot <- function(plt_filename=paste0("figure",".pdf")) {

	mprint(paste(" saving your plot in ",plt_filename))
	dev.copy(pdf,plt_filename)
	dev.off()
}

###############################################

