

getSUF <- function(table_data, table_cond){
  ####################################################
  #	Calculates Couvreur Macroscopic parameters   #
  ####################################################
  # F?licien Meunier, 06/2017
  # 
  # table_data <- rootsystem
  # table_data <-  fread("www/rootsystem2.txt", header = T)
  # setwd("../")
  
  # table_cond <- read_csv("www/conductivities.csv")
  
  ####################################################
  # Input data
  ####################################################
  prev=table_data$node1ID 	  # mother segment
  l=table_data$length    	  # segment length
  r=table_data$radius    	  # segment radius
  z=table_data$z2           # z-position
  order=table_data$type      # segment order
  seg_age= max(table_data$time) - table_data$time     # segment age
  
  Nseg=length(l)      	  # Total number of segment
  
  Psi_sr_homogeneous=-300      # Homogeneous soil-root potential 
  Psi_sr_heterogeneous=-3000   # Heterogeneous soil-root potential 
  Psi_collar=-15000            # Collar potential
  
  ####################################################
  # Interpolates kr,kx functions
  
  order_uni=unique(order)
  
  # kr=matrix(10e-3,Nseg,1) # radial conductivity of the segments
  # kx=matrix(1000,Nseg,1) # Axial conductance of the segments
  
  kr=matrix(0,Nseg,1) # radial conductivity of the segments
  kx=matrix(0,Nseg,1) # Axial conductance of the segments

  # Linear interpolation
  for ( i in 1:length(order_uni)) {

    pos=is.element(order,order_uni[i])
    od <- order_uni[i]
    if(od == 4) od <- 1 # if nodal, take value for primary

    x = table_cond$x[table_cond$order_id == od & table_cond$type == "kr"]
    y = table_cond$y[table_cond$order_id == od & table_cond$type == "kr"]
    x <- c(x, 5000)
    y <- c(y, y[length(y)])

    xout = data.frame(seg_age[pos])
    temp=data.frame(approx(x,y,xout[,1]))
    kr[pos]=temp[,2]

    x = table_cond$x[table_cond$order_id == od & table_cond$type == "kx"]
    y = table_cond$y[table_cond$order_id == od & table_cond$type == "kx"]
    x <- c(x, 5000)
    y <- c(y, y[length(y)])
    temp=data.frame(approx(x,y,xout[,1]))
    kx[pos]=temp[,2]
  }
  
  # Combination of hydraulics and geomitric properties
  kappa=sqrt(pi*r*kr*kx)  # kappa
  tau=sqrt(pi*r*kr/kx)    # tau
  
  Psi_sr=Psi_sr_homogeneous*matrix(1,Nseg,1) # Soil-root potential for each segment
  
  ####################################################
  # Build Matrices
  A = Matrix(c(0),nrow=Nseg+1,ncol=Nseg+1,sparse = TRUE) # Matrix A sparse
  
  j <- 1:Nseg
  i <- prev
  
  rows <- i+1
  columns <- i+1
  values=-kappa/sinh(tau*l)-kappa*tanh(tau*l/2);
  
  rows=c(rows,j+1)
  columns=c(columns,i+1)
  values=c(values,kappa/sinh(tau*l))
  
  rows=c(rows,i+1)
  columns=c(columns,j+1)
  values=c(values,-kappa*tanh(tau*l/2)+kappa/tanh(tau*l))
  
  rows=c(rows,j+1)
  columns=c(columns,j+1)
  values=c(values,-kappa/tanh(tau*l))
  x=mapply(values,FUN=as.numeric)
  
  A <- sparseMatrix(rows, columns, x = x) # Assignates values to specific locations
  a = A[-1,-1]				    # a matrix = A without the first line and column
  
  # Build Matrix B
  B = Matrix(c(0),nrow=Nseg+1,ncol=1,sparse = TRUE) # Matrix B sparse
  
  rows=i+1;
  columns=matrix(1,Nseg,1)
  values=-Psi_sr*kappa*tanh(tau*l/2)
  
  rows=c(rows,j+1)
  columns=c(columns,matrix(1,Nseg,1))
  values=c(values,-Psi_sr*kappa*tanh(tau*l/2))
  
  x = mapply(values,FUN=as.numeric)
  B <- sparseMatrix(rows, columns, x = x) # Assignates values to specific locations
  
  b=B[-1] # b matrix = B without the first line
  
  prev_collar=(prev==0)

  b[prev_collar] = b[prev_collar] - (Psi_collar * (kappa[prev_collar] / sinh(tau[prev_collar] * l[prev_collar])))
  
  ####################################################
  # Compute solution
  
  X=solve(a,b) 		 # a\b
  Psi_basal=X  		 # Solution = Psi_basal
  prev_temp=prev	
  prev_temp[prev==0]=1;
  Psi_proximal=Psi_basal[prev_temp] # Psi_proximal = Psi_basal of the mother segment
  Psi_proximal[prev_collar]=Psi_collar; 
  Jr=2*kappa*tanh(tau*l/2)*(Psi_sr-(Psi_proximal+Psi_basal)/2) # Total radial flow
  Jxl=kappa*((Psi_basal-Psi_sr)/sinh(tau*l)-(Psi_proximal-Psi_sr)/tanh(tau*l)); # Axial flow at the top of the segments
  
  Psi_sr_hetero=Psi_sr
  Psi_sr_hetero[z>-30]=Psi_sr_heterogeneous
  Tact_hetero=Tact/2
  Psi_sr_eq=sum(SUF*Psi_sr_hetero)
  Jr_hetero = Krs*SUF*(Psi_sr_hetero-Psi_sr_eq+Tact_hetero/Krs)  
  
  remove(a, b, A, B)
  
  # Macroscopic solution
  Tact=sum(Jr) 		 # Actual transpiration
  SUF=Jr/Tact  		 # SUF = normalized uptake
  Krs=Tact/abs(Psi_sr_homogeneous-Psi_collar) # Total root system conductance
  
  SUF[SUF < 0] <- 10e-10
  
  print(Krs)
  print(Tact)
  print(Psi_basal[1])
  
  
  ####################################################
  return(list(suf=log10(SUF), suf1 = SUF, kr = log10(kr), kx = log10(kx), tact=Tact, krs=Krs))
}
