
donuts_plot <- function(
						panel = runif(3), # counts
						pctr = c(.5,.2,.9), # percentage in count
						legend.label='',
						cols = c('chartreuse', 'chocolate','deepskyblue'), # colors
						outradius = 1, # outter radius
						radius = .7,   # 1-width of the donus 
						add = F,
						innerradius = .5, # innerradius, if innerradius==innerradius then no suggest line
						legend = F,
						pilabels=F,
						legend_offset=.25, # non-negative number, legend right position control
						borderlit=c(T,F,T,T)
						){
	par(new=add)
	if(sum(legend.label=='')>=1) legend.label=paste("Series",1:length(pctr))
	if(pilabels){
		pie(panel, col=cols,border = borderlit[1],labels = legend.label,radius = outradius)
	}
	panel = panel/sum(panel)

	pctr2= panel*(1 - pctr)
	pctr3 = c(pctr,pctr)
	pctr_indx=2*(1:length(pctr))
	pctr3[pctr_indx]=pctr2
	pctr3[-pctr_indx]=panel*pctr
	cols_fill = c(cols,cols)
	cols_fill[pctr_indx]='white'
	cols_fill[-pctr_indx]=cols
	par(new=TRUE)
	pie(pctr3, col=cols_fill,border = borderlit[2],labels = '',radius = outradius)
	par(new=TRUE)
	pie(panel, col='white',border = borderlit[3],labels = '',radius = radius)
	par(new=TRUE)
	pie(1, col='white',border = borderlit[4],labels = '',radius = innerradius)
	if(legend){
		# par(mar=c(5.2, 4.1, 4.1, 8.2), xpd=TRUE)
		legend("topright",inset=c(-legend_offset,0),legend=legend.label, pch=rep(15,'.',length(pctr)), 
			   col=cols,bty='n')
	}
	par(new=FALSE)
}
## col- > subcor(change hue/alpha)
subcolors <- function(.dta,main,mainCol){
	tmp_dta = cbind(.dta,1,'col')
	tmp1 = unique(.dta[[main]])
	for (i in 1:length(tmp1)){
		tmp_dta$"col"[.dta[[main]] == tmp1[i]] = mainCol[i]
	}
	u <- unlist(by(tmp_dta$"1",tmp_dta[[main]],cumsum))
	n <- dim(.dta)[1]
	subcol=rep(rgb(0,0,0),n);
	for(i in 1:n){
		t1 = col2rgb(tmp_dta$col[i])/256
		subcol[i]=rgb(t1[1],t1[2],t1[3],1/(1+u[i]))
	}
	return(subcol);
}
