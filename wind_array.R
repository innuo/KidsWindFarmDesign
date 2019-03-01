library(RColorBrewer)
library(spatstat)
library(fields)


domain_dims = c(200, 200)
all_inds = as.matrix(expand.grid(1:domain_dims[1], 1:domain_dims[2]))

turbine_locs = data.frame(x = numeric(0), y=numeric(0))




make_initial_wind_map = function(seed = 1){
  set.seed(seed)
  n <- round(prod(domain_dims) /5)
  W = matrix(0, nrow=domain_dims[1], ncol=domain_dims[2])
  xinds = sample(1:domain_dims[1], n, replace = T)
  yinds = sample(1:domain_dims[2], n, replace = T)
  W[cbind(xinds, yinds)] <- 20 * rnorm(n, mean = 7, sd = 2)
  W = my_smooth(W, sigma=10)
  W = W/max(W) * 10
  
  W
}

turbine_conditional_wind_map = function(turbine_xy, init_wind_map){
  x = turbine_xy[1]
  y = turbine_xy[2]
  W = matrix(0, nrow=domain_dims[1], ncol=domain_dims[2])
  W[x, y] <- 100
  W =  1 - my_smooth(W, sigma=6.0)
  W[x, y] <- 1
  turbine_wind_map = W * init_wind_map

  W = matrix(0, nrow=domain_dims[1], ncol=domain_dims[2])
  vecs = sweep(all_inds, MARGIN = 2, STATS = c(turbine_xy[1], turbine_xy[2]))
  inds = abs(vecs[,2])/(vecs[,1]) < 0.15 & vecs[,1] > 0
  vals = 0.5 * 1/(vecs[inds, 1])^0.5
  W[all_inds[inds,]] <- vals
  # 
  turbine_wind_map = turbine_wind_map * (1-W)
  turbine_wind_map
}

plot_wind_map = function(X, main){
  breaks = seq(7, 10, 0.2)
  newcol <- colorRampPalette(brewer.pal(9,"YlGnBu"))
  ncols <- length(breaks)-1
  cols <- newcol(ncols)
  
 p = image.plot(t(as.matrix(X)), col=cols, 
            breaks = breaks, 
            asp=1, axes = F, legend.shrink = 0.3, horizontal = TRUE, 
            xlim=c(0, 1), ylim=c(0, 1), main=main)
 p
}

my_smooth = function(M, sigma){
  I = im(M)
  Is = blur(I, sigma = sigma, bleed = FALSE, normalise = TRUE)
  return(Is$v)
}

