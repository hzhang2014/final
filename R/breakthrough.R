#' plot the breaktrhough curve
#' 
#' Input an excel file and print the breakthrough curve.
#' @param file name of input file
#' @return A table of raw data, and a breakthrough curve
#' @export
breakthrough <- function(file){
  #install package
  install.packages("xlsx")
  install.packages("Hmisc")
  
  #load installed package
  #package for importing .xlsx file
  library(xlsx)
  #package for adding vertical error bars
  library(Hmisc)
  
  #Import initial data
  #data <- read.xlsx("Radium.xlsx",sheetIndex=1)
  data <- read.xlsx(file,sheetIndex=1)  #import data as data.frame
  print(data)  #print out the imported data
  
  #Define molar weigth of elements, which will be used to calculate concentration in different units
  Na_MW <- 22.99
  Mg_MW <- 24.305
  Ca_MW <- 40.078
  Ba_MW <- 137.33
  Sr_MW <- 87.62
  
  #Convert test concentration got from ICP-MS to actual concentration
  #concentration of Na, Mg, Ca, Ba and Sr are in ppm unit in raw data
  #concentration of Ra is in ppt unit in raw data
  data$Na_Conc_M <- data$Na23*data$Dilution1/Na_MW/1000  #convert Na to molar unit
  data$Mg_Conc_mM <- data$Mg24*data$Dilution1/Mg_MW  #convert Mg to mmolar unit
  data$Ca_Conc_M <- data$Ca44*data$Dilution1/Ca_MW/1000  #convert Ca to molar unit
  data$Ba_Conc_mM <- data$Ba137*data$Dilution1/Ba_MW  #convert Ba to mmolar unit
  data$Sr_Conc_mM <- data$Sr88*data$Dilution1/Sr_MW  #convert Sr to mmolar unit
  data$Ra_Conc_ppt <- data$Ra226*data$Dilution2  #convert Ra to ppt unit
  
  #Calculate error value
  data$Na_error <- data$Na_Conc_M*data$Na_RSD/100
  data$Mg_error <- data$Mg_Conc_mM*data$Mg_RSD/100
  data$Ca_error <- data$Ca_Conc_M*data$Ca_RSD/100
  data$Ba_error <- data$Ba_Conc_mM*data$Ba_RSD/100
  data$Sr_error <- data$Sr_Conc_mM*data$Sr_RSD/100
  data$Ra_error <- data$Ra_Conc_ppt*data$Ra_RSD/100
  
  #Plot breakthrough curve
  par(mar=c(5,4,4,11)+.1, xpd=TRUE)
  #plot Mg breakthrough curve
  plot(data$Mg_Conc_mM~data$Reaction_Time, type="o", lwd=2, col="blue", xlab="Reaction Time [min]", ylab="Conc.Mg[mM],Ba[mM],Sr[mM],Ra[ppt]", main="Breakthrough Curve")
  #add error bar
  errbar(data$Reaction_Time,data$Mg_Conc_mM,yplus=data$Mg_Conc_mM+data$Mg_error, yminus=data$Mg_Conc_mM-data$Mg_error,add=TRUE,col="blue")
  #add Ba breakthrough curve
  points(data$Ba_Conc_mM~data$Reaction_Time, type="o", lwd=2, col="green")
  #add error bar
  errbar(data$Reaction_Time,data$Ba_Conc_mM,yplus=data$Ba_Conc_mM+data$Ba_error, yminus=data$Ba_Conc_mM-data$Ba_error,add=TRUE,col="green")
  #add Sr breakthrough curve
  points(data$Sr_Conc_mM~data$Reaction_Time, type="o", lwd=2, col="red")
  #add error bar
  errbar(data$Reaction_Time,data$Sr_Conc_mM,yplus=data$Sr_Conc_mM+data$Sr_error, yminus=data$Sr_Conc_mM-data$Sr_error,add=TRUE,col="red")
  #add Ra breakthrough cuve
  points(data$Ra_Conc_ppt~data$Reaction_Time, type="o", lwd=2, col="yellow")
  #add error bar
  errbar(data$Reaction_Time,data$Ra_Conc_ppt,yplus=data$Ra_Conc_ppt+data$Ra_error, yminus=data$Ra_Conc_ppt-data$Ra_error,add=TRUE,col="yellow")
  
  #plot breakthrough curve on secondary y-axis
  #Since different range of concentrations, we need a secondary axis to have a better plot
  par(new=TRUE)
  #plot Na concentration in secondary y-axis
  plot(data$Na_Conc_M~data$Reaction_Time, type="o", lwd=2, col="orange",xaxt="n", yaxt="n", xlab="", ylab="")
  #add error bar
  errbar(data$Reaction_Time,data$Na_Conc_M,yplus=data$Na_Conc_M+data$Na_error, yminus=data$Na_Conc_M-data$Na_error,add=TRUE,col="orange")
  #add Ca concentration in secondary y-axis
  points(data$Ca_Conc_M~data$Reaction_Time, type="o", lwd=2, col="purple")
  #add error bar
  errbar(data$Reaction_Time,data$Ca_Conc_M,yplus=data$Ca_Conc_M+data$Ca_error, yminus=data$Ca_Conc_M-data$Ca_error,add=TRUE,col="purple")
  #add secondary y-axis
  axis(side=4)  #add axis on the right
  mtext("Conc.Na[M],Ca[M]",side=4,line=3)  #add label of secondary y-axis
  #add legend outside the plot
  legend("right", inset=c(-0.5,0), legend=c("Na","Mg","Ca","Ba","Sr","Ra"), cex=1.05, col=c("orange","blue","purple","green","red","yellow"), lty=1, lwd=2, pch=19, bg="transparent", box.col="transparent")
  
}








#' plot the breaktrhough curve with normalized time and concentration
#' 
#' Input an excel file and print the normalized breakthrough curve.
#' @param file name of input file
#' @param bed_volume volume of resin column, default value is 12
#' @return A table of raw data, and a breakthrough curve
#' @export
breakthrough.norm <- function(file, bed_volume=12){
  #install package
  install.packages("xlsx")
  install.packages("Hmisc")
  
  #load installed package
  #package for import .xls file
  library(xlsx)
  #package for vertical error bars
  library(Hmisc)
  
  
  #Import initial data
  data <- read.xlsx(file,sheetIndex=1)  #import data as data.frame
  print(data)
  
  #Convert test concentration to normalized concentration
  data$Na_norm <- (data$Na23)/(data$Na23)[length(data$Na23)]
  data$Mg_norm <- (data$Mg24)/(data$Mg24)[length(data$Mg24)]
  data$Ca_norm <- (data$Ca44)/(data$Ca44)[length(data$Ca44)]
  data$Ba_norm <- (data$Ba137)/(data$Ba137)[length(data$Ba137)]
  data$Sr_norm <- (data$Sr88)/(data$Sr88)[length(data$Sr88)]
  data$Ra_norm <- (data$Ra226)/(data$Ra226)[length(data$Ra226)]
  
  #Convert reaction time to normalized time
  #calculate average flow rate and volume
  flow.avg <- mean(data$Flow_Rate[1:length(data$Flow_Rate)-1])
  #convert to normalized time (No. bed volume)
  data$time_norm <- data$Reaction_Time*flow.avg/bed_volume
  
  #Calculate error value
  data$Na_error <- data$Na_norm*data$Na_RSD/100
  data$Mg_error <- data$Mg_norm*data$Mg_RSD/100
  data$Ca_error <- data$Ca_norm*data$Ca_RSD/100
  data$Ba_error <- data$Ba_norm*data$Ba_RSD/100
  data$Sr_error <- data$Sr_norm*data$Sr_RSD/100
  data$Ra_error <- data$Ra_norm*data$Ra_RSD/100
  
  #Plot breakthrough curve
  par(mar=c(5,4,4,11)+.1, xpd=TRUE)
  #plot Na breakthrough curve
  plot(data$Na_norm~data$time_norm, type="o", lwd=2, col="orange", xlab="#Bed Volume [L/L]", ylab="Norm. Conc.[M/M]", main="Breakthrough Curve--Normalized")
  #add error bar
  errbar(data$time_norm,data$Na_norm, yplus=data$Na_norm+data$Na_error, yminus=data$Na_norm-data$Na_error,add=TRUE,col="orange")
  #add Mg breakthrough curve
  points(data$Mg_norm~data$time_norm, type="o", lwd=2, col="blue")
  #add error bar
  errbar(data$time_norm, data$Mg_norm, yplus=data$Mg_norm+data$Mg_error, yminus=data$Mg_norm-data$Mg_error,add=TRUE,col="blue")
  #add Sr breakthrough curve
  points(data$Sr_norm~data$time_norm, type="o", lwd=2, col="red")
  #add error bar
  errbar(data$time_norm,data$Sr_norm, yplus=data$Sr_norm+data$Sr_error, yminus=data$Sr_norm-data$Sr_error,add=TRUE,col="red")
  #add Ra breakthrough curve
  points(data$Ra_norm~data$time_norm, type="o", lwd=2, col="yellow")
  #add error bar
  errbar(data$time_norm,data$Ra_norm, yplus=data$Ra_norm+data$Ra_error, yminus=data$Ra_norm-data$Ra_error,add=TRUE,col="yellow")
  #add Ca breakthrough curve
  points(data$Ca_norm~data$time_norm, type="o", lwd=2, col="purple")
  #add error bar
  errbar(data$time_norm,data$Ca_norm, yplus=data$Ca_norm+data$Ca_error, yminus=data$Ca_norm-data$Ca_error,add=TRUE,col="purple")
  #add normalized influent as reference
  segments(x0=0,x1=data$time_norm[length(data$time_norm)-1], y0=1, y1=1, col="black", lty=2, lwd=2)
  
  #plot breakthrough curve on secondary y-axis
  par(new=TRUE)
  #plot Ba breakthrough curve
  plot(data$Ba_norm~data$time_norm, type="o", lwd=2, col="green",xaxt="n", yaxt="n", xlab="", ylab="")
  #add error bar
  errbar(data$time_norm, data$Ba_norm, yplus=data$Ba_norm+data$Ba_error, yminus=data$Ba_norm-data$Ba_error,add=TRUE,col="green")
  #add secondary y-axis
  axis(side=4)  #add secondary axis on the right
  mtext("Conc.Na[M],Ca[M]",side=4,line=3)  #add label of secondary y-axis
  #add normalized influent as reference
  segments(x0=0,x1=data$time_norm[length(data$time_norm)-1], y0=1, y1=1, col="black", lty=2, lwd=2)
  
  #add legend outside the plot on the right
  legend("right", inset=c(-0.55,0), legend=c("Na","Mg","Ca","Ba","Sr","Ra","Influent"), cex=1.05, col=c("orange","blue","purple","green","red","yellow","black"), lty=c(1,1,1,1,1,1,2), lwd=2, pch=c(19,19,19,19,19,19,NA), bg="transparent", box.col="transparent")
  
}









#' caculate ion-exchange capacity of a chemical
#' 
#' Input an excel file, the weight of resin and which chemical element you are interested, and output the ion-exchange capacity.
#' @param file name of input file
#' @param weight weight of the resin
#' @param element element you are interested, options: Na, Mg, Ca, Ba, Ra, Sr
#' @return A sentence with the name of chemical element and its capacity
#' @export
#Function to calculate ion-exchange capacity--Capacity(file name, weight of resin, interested element)
capacity <- function(file, weight,element){
  #Calculate ion-exchange capacity of Ra
  #import data
  #check package
  install.packages("xlsx")
  #loading installed package
  #package for import .xls file
  library(xlsx)
  data <- read.xlsx(file,sheetIndex=1)
  
  #calculate average flow rate
  flow.avg <- mean(data$Flow_Rate[1:length(data$Flow_Rate)-1])
  
  #calculate the volume of water that flows through the column
  volume <- data$Reaction_Time[1:length(data$Reaction_Time)-1]*flow.avg
  
  #Define molar weigth of elements
  Na_MW <- 22.99
  Mg_MW <- 24.305
  Ca_MW <- 40.078
  Ba_MW <- 137.33
  Sr_MW <- 87.62 
  
  #calculate delta_conc (concentration change)
  if(element=="Ra"){
    conc <- (data$Ra226*data$Dilution2)[1:length(data$Ra226)-1]  #actual effluent concentration of Ra
    influent <- (data$Ra226*data$Dilution2)[length(data$Ra226)]  #actual influent concentration of Ra
  } else if(element=="Na"){
    conc <- (data$Na23*data$Dilution1/Na_MW)[1:length(data$Na23)-1]  #actual effluent concentration of Na
    influent <- (data$Na23*data$Dilution1/Na_MW)[length(data$Na23)]  #actual influent concentration of Na
  } else if(element=="Mg"){
    conc <- (data$Mg24*data$Dilution1/Mg_MW)[1:length(data$Mg24)-1]  #actual effluent concentration of Mg
    influent <- (data$Mg24*data$Dilution1/Mg_MW)[length(data$Mg24)]  #actual influent concentration of Mg
  } else if(element=="Ca"){
    conc <- (data$Ca44*data$Dilution1/Ca_MW)[1:length(data$Ca44)-1]  #actual effluent concentration of Ca
    influent <- (data$Ca44*data$Dilution1/Ca_MW)[length(data$Ca44)]  #actual influent concentration of Ca
  } else if(element=="Ba"){
    conc <- (data$Ba137*data$Dilution1/Ba_MW)[1:length(data$Ba137)-1]  #actual effluent concentration of Ba
    influent <- (data$Ba137*data$Dilution1/Ba_MW)[length(data$Ba137)]  #actual influent concentration of Ba
  } else {
    conc <- (data$Sr88*data$Dilution1/Sr_MW)[1:length(data$Sr88)-1]  #actual effluent concentration of Sr
    influent <- (data$Sr88*data$Dilution1/Sr_MW)[length(data$Sr88)]  #actual influent concentration of Sr
  }
  
  delta_conc <- influent - conc  #calculate concentration changes
  
  #calculate delta_volume (volume change)
  delta_volume <- NA  #pre-define delta_volume
  for(i in 1:length(data$Ra226)-1)
  {
    if(i==1){
      delta_volume[i] <- volume[i]
    } else  delta_volume[i] <- volume[i]-volume[i-1]
  }
  
  #calculate amount of ion exchange
  amount <- sum(delta_conc*delta_volume/1000)  #ion-exchange amount
  
  
  #calculate ion-exchange capacity
  capacity <- amount/weight
  #define the unit
  if(element=="Ra"){
    unit <- "nCi/g"  
  } else {unit <- "m moles/g"}
  element.capacity <- paste("Ion-exchange capacity for",element,"is",capacity,unit)
  
  return(element.capacity)
}
