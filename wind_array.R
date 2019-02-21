library(RColorBrewer)
library(spatstat)
library(fields)


domain_dims = c(200, 200)
all_inds = as.matrix(expand.grid(1:domain_dims[1], 1:domain_dims[2]))

turbine_locs = data.frame(x = numeric(0), y=numeric(0))




make_initial_wind_map = function(seed = 2){
  set.seed(seed)
  W = matrix(runif(prod(domain_dims)), nrow=domain_dims[1])
  W = my_smooth(W, sigma=5)
  #
  W = W/max(W)
  W = W * 10
  W
}

turbine_conditional_wind_map = function(turbine_xy, init_wind_map){
  x = turbine_xy[1]
  y = turbine_xy[2]
  W = matrix(0, nrow=domain_dims[1], ncol=domain_dims[2])
  W[x, y] <- 1
  W =  1 - my_smooth(W, sigma=3.5)
  W[x, y] <- 1
  turbine_wind_map = W * init_wind_map

  W = matrix(0, nrow=domain_dims[1], ncol=domain_dims[2])
  vecs = sweep(all_inds, MARGIN = 2, STATS = turbine_xy)
  inds = abs(vecs[,2])/(vecs[,1]) < 0.1 & vecs[,1] > 0
  vals = 0.3 * 1/sqrt(vecs[inds, 1])
  W[all_inds[inds,]] <- vals
  # 
  turbine_wind_map = turbine_wind_map * (1-W)

  turbine_wind_map
}

plot_wind_map = function(X){
  breaks = seq(7, 10, 0.2)
  newcol <- colorRampPalette(brewer.pal(9,"YlGnBu"))
  ncols <- length(breaks)-1
  cols <- newcol(ncols)
  
 p = image.plot(t(as.matrix(X)), col=cols, 
            breaks = breaks, 
            asp=1, axes = F, legend.shrink = 0.3, horizontal = TRUE, 
            xlim=c(0, 1), ylim=c(0, 1))
 p
}

my_smooth = function(M, sigma){
  I = im(M)
  Is = blur(I, sigma = sigma, bleed = FALSE, normalise = TRUE)
  return(Is$v)
}

