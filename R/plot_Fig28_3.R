#' Replicate 3D Figure 28.3 varying summar and winter carrying capacities (K)
#'
#' Process the output of runMultiFAC() to re-create the 3D Fig 28.3
#'
#' @param runFAC.multi output from runFAC_multi()
#' @param plot.type 3D perspective plot ("P"; default) or 2D contour plot ("C")
#' @param scale. include scale
#' @param tick.labs ????
#'
#' @import graphics
#'
#' @export

plot_Fig28_3 <- function(runFAC.multi,
                         plot.type ="P",
                         scale. = T,
                         tick.labs = 1){

  #Reshape output to make 2D and persp plots
  cast.out <- reshape2::dcast(data = runFAC.multi,
                    formula = K.bc.i ~ K.wg.i,
                    value.var = "B.f.tot", #B.tot
                    fun.aggregate = sum)

  #create x and y axis
  x.K.bc.i <- cast.out$K.bc.i
  y.K.wg.i <- as.numeric(names(cast.out)[-1])

  #create z matrix; this is necessary b/c of formatting problems created by dcast()
  z0 <- cast.out[,-1] #unprocessed data
  z1 <- matrix(data = NA, nrow = dim(z0)[1], ncol = dim(z0)[1]) #blank matrix

  #function to rebuild z matirx
  fx <- function(){for(j in 1:dim(z1)[1]){
    for(i in 1:dim(z1)[2]){
      z1[j,i] <- z0[[j,i]]
    }
  }
    return(z1)
  }

  #create z matrix
  z1 <- fx()

  # Make perspective plot
  if(plot.type %in% c("perspective","persp","p","P")){
    graphics::persp(col = 3,
          shade = 0.25,
          scale = scale.,
          ticktype = "detailed",
          cex.lab = tick.labs,
          nticks =3,
          #cex.axis = 0.5,
          #cex = 0.5,
          #mex = 0.5,
          y = y.K.wg.i ,
          x = x.K.bc.i,
          z  = (z1),
          ylab = "K.wg", #K.bc.i
          xlab = "", #x.K.bc.i", #K.wg.i
          zlab = "", #B
          r = sqrt(4),
          d = 1,
          theta = -50,
          phi = 15#,
          #zlim = c(0,2000)#2000 total indivials = 1000 PAIRS
    )
    #xE <- c(-1000,1000); xy <- expand.grid(xE, xE)
    #points(trans3d(xy[,1], xy[,2], 200), col = 2, pch = 16)

  }

  if(plot.type %in% c("contour","cont","c","C")){
    contour.out <-graphics::contour(x = x.K.bc.i,
                          y = y.K.wg.i,
                          z  = t(z1))

  }
  # scatterplot3d(x = x$K.bc.i,
  #               y  = x$K.wg.i,
  #               z  = x$B.m.tot,
  #               #angle = 210,
  #               type = "p",
  #               scale.y = 1,
  #               pch=16, highlight.3d=F,
  #               grid = F)

}
