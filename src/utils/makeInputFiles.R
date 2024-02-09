## make some netcdf files for playing by munging existing file

## useful webpage:
## https://pjbartlein.github.io/REarthSysSci/netCDF.html#introduction

library(ncdf4)

initial_args <- commandArgs(trailingOnly = FALSE)

file_arg_name = "--file="
script_dir = dirname(sub(file_arg_name, "", initial_args[grep(file_arg_name, initial_args)]))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 1) { 
    input_file=args[1]
    output_file = file.path(dirname(input_file), paste("output_", basename(input_file), collapse=""))
} else if (length(args) == 2) {
    input_file=args[1]
    output_file=args[2]
} else {
    stop("Usage: makeInputFiles.R <input_file.nc> [<output_file.nc]")
}

## 01/10/1979 to 31/12/2016 - 38 years

## look at forcing data for Harp lake:
## DataFolder='/home/helen/GitLab/NERC2023/ALBM/OriginalFiles/Data/'
## fname='forcing_obs_Harp.nc'

## makeNewFile=FALSE
## newDataFolder='/home/helen/GitLab/NERC2023/ALBM/Windermere/ForcingData/'
## newfname='forcing_obs_Harp.nc'

loadNCValues=function(nc){

    var.names = names(nc$var)
    Nv = nc$nvars

    longNames = rep(NA,Nv)
    units = rep(NA,Nv)
    fillvalues = rep(NA,Nv)

    for (v in 1:Nv){
        units[v] = ncatt_get(nc,var.names[v],'units')$value
        longNames[v] = ncatt_get(nc,var.names[v],'long_name')$value
        fillvalues[v] = ncatt_get(nc,var.names[v], "_FillValue")$value
    }

    ## get the time dimension (must be a better way!)
    tas = ncvar_get(nc, 'tas')
    L=dim(tas)
    time= 1:L

    nclist=list()
    nclist$var.names=var.names
    nclist$units=units
    nclist$longNames=longNames
    nclist$fillvalues=fillvalues
    nclist$time=time
    nclist$Nv=Nv
    
    return(nclist)
}

#------------------------------------------------------------------

#load met file
nc = nc_open(input_file)
ncIN=loadNCValues(nc)
time=ncIN$time
print(cbind(ncIN$longNames,ncIN$units))

#Make new netcdf file------------------------------------

## define dimensions
timedim <- ncdim_def("time",'d',as.double(time))

vlist=list()
v=1 ## date has no dimensions
vnameDef=paste0(ncIN$var.names[v],'_def')

assign(vnameDef,
       ncvar_def(ncIN$var.names[v],
                 ncIN$units[v],NULL,
                 ncIN$fillvalues[v],
                 ncIN$longNames[v],
                 prec='integer',shuffle=FALSE,
                 compression=NA, chunksizes=NA,
                 verbose=FALSE ))
vlist[[1]]=get(vnameDef)

for (v in 2:ncIN$Nv){
    vnameDef=paste0(ncIN$var.names[v],'_def')
    assign(vnameDef,
           ncvar_def(ncIN$var.names[v],ncIN$units[v],timedim,
                     ncIN$fillvalues[v],ncIN$longNames[v],
                     prec='float',
                     shuffle=FALSE, compression=NA,
                     chunksizes=NA, verbose=FALSE )
           )
    vlist[[v]]=get(vnameDef)
}

## Next, create the file, and put the variables into it, along with
## additional variable and “global” attributes (those that apply to
## the whole file). Note that the attributes are of key importance to
## the self-documenting properties of netCDF files.
ncout <- nc_create(output_file,vlist)

## put variables
for (v in 1:ncIN$Nv){
    obj=get(paste0(ncIN$var.names[v],'_def'))
    varName=ncIN$var.names[v]
    if (varName%in%c('tas','tasmin','tasmax')){ 
        varVal=ncvar_get(nc,varName)+15 #add 15oC to temperatures 
    }else if (varName=='prsn'){ ## Disable snowfall
        varVal=ncvar_get(nc,varName)*0
    }else{
        varVal=ncvar_get(nc,varName)
    }        
    ncvar_put(ncout,obj,varVal)
}
    

## put additional attributes into dimension and data variables
## ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
## ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")
    
ncatt_put(ncout,0,"source",'Helen Kettle')
ncatt_put(ncout,0,"comment",'Testing!')
    
ncout
    
nc_close(ncout)
cat(sprintf("Wrote modified observations from %s to %s\n", input_file, output_file))

