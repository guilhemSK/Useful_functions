# Multiple curves plot:
# Takes as input a matrix of curves for y (one column per curve, or a single vector if only one curve),
# and either a matrix of x values (one column per curve), or a single x vector if all curves share the same x values
# requires ggplot and ggforce

curv.plot <- function(x,y,col.vect=NULL,leg.name="Legend",
                     x.lab,y.lab,y.lab.hjust=0.5,size.factor=1,
                     x.log=F,y.log=F,rev.x=F,rev.y=F,
                     mar.vect=c(5,5,5,5),letter=NULL,position.letter.xy=c(0.1,0.9),letter.size=12)
{
  x[is.infinite(x)] = NA
  y[is.infinite(y)] = NA
  
  if (x.log)
    x[x == 0] = NA
  if (y.log)
    y[y == 0] = NA
  
  # If one x vector is given for several y columns, x is transformed into a matrix of the same dimension as x
  if (is.null(dim(x)) && is.null(dim(y)))
  {
    if (length(x) != length(y))
      stop("x and y should have the same length")
    else
      length.x = length(x)
  } else if (is.null(dim(x)) && !is.null(dim(y)))
  {
    if (length(x) != nrow(y))
    {
      stop("x should have length nrow(y)")
    } else
    {
      length.x = length(x)
      x = matrix(data = rep(x,ncol(y)), ncol = ncol(y))
      line.factor = rep(1:ncol(y),each=length.x)
    }
  } else if (!is.null(dim(x)) && !is.null(dim(y)))
  {
    if (nrow(x) != nrow(y) || ncol(x) != ncol(y))
    {
      stop("x and y should have the same dimensions")
    } else
    {
      length.x = nrow(x)
      line.factor = rep(1:ncol(y),each=length.x)
    }
  } else
    stop("Invalid x and y dimensions")
  
  if (!is.null(dim(y)))
    line.factor = rep(1:ncol(y),each=length.x)  
  else 
    line.factor = rep(1,times=length.x)
  
  if (is.null(col.vect))
  {
    plot = ggplot(data = data.frame(x = as.vector(x), y = as.vector(y), group = line.factor)) + 
      geom_point(aes(x,y), size = 1*size.factor) +
      geom_line(aes(x,y,group), size = 1*size.factor)
  } else
    # requires library(colorspace):
    plot = ggplot(data = data.frame(x = as.vector(x), y = as.vector(y), group = line.factor)) + 
            geom_point(aes(x,y, colour = factor(group)), size = 1*size.factor) +
            geom_line(aes(x,y,group=group, colour=factor(group)), size = 1*size.factor) +
            scale_colour_manual(values = col.vect,
                                labels = colnames(y),
                                name = leg.name) +
            guides(colour = guide_legend(override.aes = list(size=3*size.factor)))
  
  plot = plot +
    theme_bw() +
    theme(axis.text = element_text(size=22*size.factor),
          axis.title=element_text(size=22*size.factor),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24*size.factor),
          plot.margin=unit(mar.vect,"mm")) +
    labs(x=x.lab, y=y.lab)
  
  if (x.log)
  {
    plot = plot + scale_x_continuous(trans = if (rev.x) trans_reverser('log10') else "log10")
    if (!is.null(letter))
      letter.x.pos = (max(x,na.rm=T)/min(x,na.rm=T))^position.letter.xy[1]*min(x,na.rm=T)
  } else
  {
    if (!is.null(letter))
      letter.x.pos = position.letter.xy[1]*(max(x,na.rm=T)-min(x,na.rm=T))+min(x,na.rm=T)
    if (rev.x)
      plot = plot + scale_x_continuous(trans = "reverse")
  }
  
  if (y.log)
  {
    plot = plot + scale_y_continuous(trans = if (rev.y) trans_reverser('log10') else "log10")
    if (!is.null(letter))
      letter.y.pos = (max(y,na.rm=T)/min(y,na.rm=T))^position.letter.xy[2]*min(y,na.rm=T)
  } else
  {
    if (!is.null(letter))
      letter.y.pos = position.letter.xy[2]*(max(y,na.rm=T)-min(y,na.rm=T))+min(y,na.rm=T)
    if (rev.y)
      plot = plot + scale_y_continuous(trans = "reverse")
  }
  
  if (!is.null(letter))
  {
    plot = plot + annotate(geom="text",
                           x = letter.x.pos,
                           y = letter.y.pos,
                           label=letter,
                           # label = bquote("Median"==.(format(median.adjR2.per.MEM[k],digits=2))),
                           hjust = 1,
                           size=letter.size*size.factor)
  }
  return(plot)
}
