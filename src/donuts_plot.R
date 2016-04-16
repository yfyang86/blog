resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


donuts_plot <- function(panel = runif(3), # counts
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


## example: simple example
data_example = read.table(text=
                            "year all pctr
                          2012 123182 .213
                          2013 297821 .43
                          2013 512332 .55
                          2014 455213 .88
                          ",header=T)

cols = c('chartreuse', 'chocolate','deepskyblue')
cols_u = c(list(cols),lapply(1:4,function(x){paste0(cols,x)}))

donuts_plot(data_example$all, data_example$pctr,data_example$year,cols=c('chartreuse', 'chocolate','deepskyblue','gold'),legend=T,legend_offset=-.02)
donuts_plot(runif(3),outradius = .7,radius = .4,innerradius=.3,add=T,cols = cols_u[[4]],legend = F)





mainCol =  c('chartreuse', 'chocolate','deepskyblue','gold','deeppink')

## example: Browsers


browsers <- structure(list(browser = structure(c(3L, 3L, 3L, 3L, 2L, 2L, 
                                                 2L, 1L, 5L, 5L, 4L), 
                                               .Label = c("Chrome", "Firefox", "MSIE","Opera", "Safari"),class = "factor"), 
                           version = structure(c(5L,6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L), 
                                               .Label = c("Chrome 10.0", "Firefox 3.5", "Firefox 3.6", "Firefox 4.0", "MSIE 6.0", 
                                                          "MSIE 7.0","MSIE 8.0", "MSIE 9.0", "Opera 11.x", "Safari 4.0", "Safari 5.0"),
                                               class = "factor"), 
                           share = c(10.85, 7.35, 33.06, 2.81, 1.58,13.12, 5.43, 9.91, 1.42, 4.55, 1.65), 
                           ymax = c(10.85, 18.2, 51.26,54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73), 
                           ymin = c(0,10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53,90.08)),
                      .Names = c("browser", "version", "share", "ymax", "ymin"),
                      row.names = c(NA, -11L), class = "data.frame")
browsers=browsers[order(browsers$browser,browsers$share),]
arr=aggregate(share~browser,browsers,sum)

pdf("W:/example.pdf",width = 9,height = 7)
donuts_plot(arr$share,rep(1,5),arr$browser,cols=mainCol,pilabels=T,legend=F)
donuts_plot(browsers$share,rep(1,11),browsers$version,cols=subcolors(browsers,"browser",mainCol),pilabels=T,legend_offset=.02,outradius = .71,radius = .4,innerradius=.3,add=T)

donuts_plot(browsers$share,rep(1,11),browsers$version,cols=subcolors(browsers,"browser",mainCol),legend=F,pilabels = T,borderlit = rep(F,4) )
donuts_plot(arr$share,rep(1,5),arr$browser,cols=mainCol,pilabels=F,legend=T,legend_offset=-.02,outradius = .71,radius = .0,innerradius=.0,add=T,borderlit = rep(F,4) )

dev.off()
#par(resetPar())
