
library(ncdf4)
library(pheatmap)
library(gridExtra)
library(ggplot2)

saveFigs=FALSE

if (sys.nframe() == 0L) {
    initial_args <- commandArgs(trailingOnly = FALSE)

    file_arg_name = "--file="
    script_dir = dirname(sub(file_arg_name, "", initial_args[grep(file_arg_name, initial_args)]))
    source(file.path(script_dir, "nameList.R"))
    
    args <- commandArgs(trailingOnly = TRUE)   

    if (length(args) == 1) {
        nml_data = suppressWarnings(read_nml(args[1]))
    } else {
        stop("Usage: plotOutputs.R <nameList.bLake>")
    }    
} else {
    
    for (dev in dev.list()) {
        dev.off()
    }
    
    nml_data = suppressWarnings(read_nml("mesocosm.bLake"))
}


results.folder=nml_data$archive_dir
lake_start = nml_data$general$lake_range[1]
lake_end = nml_data$general$lake_range[2]

if (lake_start != lake_end) {
    stop("Plotting multiple lakes at once is not yet supported. Set lake_range start and end to be the same number")
}

Lake=lake_start
results.folder= nml_data$archive$archive_dir

if (!dir.exists(results.folder)) {
    stop(sprintf("Results directory %s does not exist", results.folder))
}

spinup=365*0+1 #WTF?
start.date=sprintf("%04d%02d%02d",nml_data$simulation$Start_Year, nml_data$simulation$Start_Month, nml_data$simulation$Start_Day)
end.date=sprintf("%04d%02d%02d",nml_data$simulation$End_Year, nml_data$simulation$End_Month, nml_data$simulation$End_Day)
tstep=1 #day

twoDfiles=paste0('bLakeOut.',
    c('fch4d','fch4e','fco2','icethick','lakeheatf','latentheatf','lwup',
    'momf','sedheatf','snowthick','swdw','swup'),'.',
    start.date,'_',end.date,'.nc')
    
threeDfiles=paste0('bLakeOut.',
    c('chl','dch4','dco2','do','doc','phytobio','srp','turbdiffheat','watertemp'),
    '.',start.date,'_',end.date,'.nc')

zfile='bLakeOut.zw.nc'
co2file=paste0('bLakeOut.fco2.',start.date,'_',end.date,'.nc')


loadNCfile=function(nc_fname){#----------------------------------

    #load netcdf file
    nc = nc_open(nc_fname)
        
    var.name=names(nc$var)
        
    var = ncvar_get(nc, var.name)
    
    fillvalue= ncatt_get(nc,var.name, "_FillValue")$value

    var[var==fillvalue] = NA

    units=ncatt_get(nc,var.name,'units')$value
    long.name=ncatt_get(nc,var.name,'long_name')$value
    dim.names=attributes(nc$dim)$names

    if (!sum(var,na.rm=TRUE)>0){
        print(paste0(nc_fname,' i.e. ',long.name,', has no data!'))
        status='empty'
    }else{
        status='good'
    }
 
    nc_close(nc)
    
    return(list(status=status,var=var,vname=var.name,units=units,
                dim.names=dim.names,long.name=long.name))
}

linePlotAgainstTime=function(time,x,Lake){#------------------------

    var=x$var
    vname=x$vname
    vunits=x$units
    long.name=x$long.name
    dims=dim(var)
    Ld=length(dims)

    if (Ld==2){ #[T,Lake]
        vec=var[,Lake]
                #ii=seq(spinup,min(spinup+365,length(time)))
        plot(time,vec,type='l',ylab=paste0(vname,' (',vunits,')'),
             xlab='time',main=long.name)
                
    }else if (Ld==3){ #[Z,T,Lake]
                #just plot a few depths
                #ii=seq(spinup,min(spinup+365,dims[2]))
        mat=var[,,Lake]
        levels=seq(1,nrow(mat),10) ##TODO: doesn't like a single layer - step is too large
        cols=colfunc(length(levels))

        ## t(mat[levels,]) - change for single layer plot
        if (nrow(mat[levels,]) > 1) {
            trans_func = t
        } else {
            trans_func = as.matrix
        }
        matplot(time,trans_func(mat[levels,]),col=cols,lty=1,type='l',
                main=paste0(long.name),
                ylab=paste0(vname,' (',vunits,')'),
                xlab='time')
        
    }
}

#---------------------------------------------------------------------

Lf=c(length(twoDfiles),length(threeDfiles))

depth=loadNCfile(paste0(results.folder,zfile))$var[,Lake]
dz=diff(depth)

time=seq(1,nrow(loadNCfile(paste0(results.folder,co2file))$var))*tstep

colfunc=colorRampPalette(c('cyan','black'))

for (g in 1:2){

    if (g==1){
        f.list=twoDfiles
        dev.new(height=8,width=10)
        par(mfrow=c(4,3),mar=c(5,4,2,1))
    }else{
        f.list=threeDfiles
        dev.new(height=8,width=8)
        par(mfrow=c(3,3),mar=c(5,4,2,1))
    }
    
    for (f in 1:Lf[g]){
    
        file = f.list[f]
        nc_fname = paste0(results.folder,file)
        x=loadNCfile(nc_fname)
        
        if (x$status=='good'){
            var=x$var
            vname=x$vname
            vunits=x$units
            long.name=x$long.name
            dims=dim(var)
            Ld=length(dims)
            
            if ('Time'%in%x$dim.names){ 
                linePlotAgainstTime(time,x,Lake)
            }
        }
    }
    if (saveFigs){
        dev.copy2pdf(file=paste0('Figures/Lake',Lake,'TimeLinePlot',g,'.pdf'))
    }
}



#cols=colfunc(12)
cols=rainbow(12)

dev.new(width=10,height=9)#width=Lf3*5,height=5)
#par(mfrow=c(1,Lf3),mar=c(5,4,2,1))
par(mfrow=c(3,3),mar=c(5,4,2,1))


for (file in threeDfiles){
    
    nc_fname = paste0(results.folder,file)
    
    x=loadNCfile(nc_fname)
    var=x$var
    vname=x$vname
    vunits=x$units
    long.name=x$long.name

    mat=var[,,Lake]

    dims=dim(mat)

    month_indexes=spinup+round(seq(15,366,30))

    ii=depth<25 ##TODO: 25 should not be hard-coded
    if (nrow(mat[ii,]) > 1) {
        trans_func = function (x) { x }
    } else {
        trans_func = function (x) { t(as.matrix(x)) }
    }
    
    matplot(trans_func(mat[ii,month_indexes]),as.matrix(-depth[ii]),type='l', ##TODO: depth is messed up here due to having single layer
            main=long.name,lty=1:12,
            xlab=paste0(vname,' (',vunits,')'),ylab='depth',
            col=cols,lwd=2,
            #xlim=c(min(mat[ii,month_indexes]),1.2*max(mat[ii,month_indexes]))
            )

    if (vname=='dch4'){
        legend('bottomleft',legend=ceiling((month_indexes-spinup)/30.4),col=cols,lty=1:12,
               lwd=2,title='month',bty='n',cex=1.)
    }
}
if (saveFigs){
    dev.copy2pdf(file=paste0('Figures/Lake',Lake,'DepthLinePlot.pdf'))
}
#================plot heatmap=========================
plotList=list()

ct=1
for (f in 1:Lf[2]){

    file = threeDfiles[f]
    nc_fname = paste0(results.folder,file)
    
    x=loadNCfile(nc_fname)
    var=x$var
    vname=x$vname
    vunits=x$units
    long.name=x$long.name

#    if (vname!='turbdiffheat'){ #don't plot this
    mat=var[,,Lake]

    dims=dim(mat)

    rownames(mat)=depth
    colnames(mat)=time
    df=reshape2::melt(mat,varnames=c('depth','time'))
    df$height=c(diff(depth),max(diff(depth)))
    
    gg = ggplot(df, aes(time, -depth, height = -height)) +
       geom_tile(aes(fill = value),show.legend=TRUE)+
           scale_fill_gradientn(
               colours = c("black", "darkblue","blue","lightblue","lightyellow", "orange","red"),
               breaks = signif(seq(min(mat),max(mat),length.out=6),3))+
                   xlab('Time (d)')+ylab('Depth (m)')+ggtitle(long.name)+
                       guides(fill=guide_legend(title=vname))+
                           theme(legend.text = element_text(size=7),
                                 legend.title = element_text(size=10))

    #print(gg)
    plotList[[ct]]=gg
    ct=ct+1
#}
}


dev.new(width=15,height=10)
grid.arrange(grobs=plotList, ncol=3,nrow=3)
if (saveFigs){
    dev.copy2pdf(file=paste0('Figures/Lake',Lake,'heatmaps.pdf'))
}
#pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE)
#pheatmap(matC, cluster_rows = FALSE, cluster_cols = FALSE)

