cor.plot <- function(x,y,excluded.points=NULL,
                     x.lab,y.lab,y.lab.hjust=0.5,
                     x.log=F,y.log=F,fit=T,x.cor.pos=0.8,y.cor.pos=0.1,
                     mar.vect=c(5,5,5,5),letter=NULL,position.letter.xy=c(0.1,0.9))
{
  x[is.infinite(x)] = NA
  y[is.infinite(y)] = NA
  
  if (x.log)
    x[x == 0] = NA
  if (y.log)
    y[y == 0] = NA
  
  plot = ggplot(data = data.frame(x = x, y = y)) +
    # geom_point(aes(x,y1)) +
    geom_point(aes(x,y)) +
    # scale_x_log10() +
    # scale_y_log10() +
    theme_bw() +
    # ggtitle(LETTERS[6]) +
    # geom_hline(yintercept = 1, linetype="dashed") +
    # ggtitle("RDA adjR2 3 vs 2 abiotic PCA axes with stdzation") +
    # ggtitle("Pure abiotic vs. total abiotic adjR2 3 axes") +
    theme(axis.text = element_text(size=22),
          axis.title=element_text(size=22),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24),
          plot.margin=unit(mar.vect,"mm")) +
    #labs(x="Mean size (micron)", y=paste("Ratio of",c("surface","DCM")[i_case],"variance\n purely explained by currents vs. envir.")) +
    labs(x=x.lab, y=y.lab)
  
  x.fit = x.not.excluded = x
  y.fit = y.not.excluded = y
  if (length(excluded.points)>0)
  {
    x.excluded = x
    y.excluded = y
    # Plotting in red the points that are excluded from fitting:
    x.excluded[-excluded.points] = NA
    y.excluded[-excluded.points] = NA
    plot = plot +
      geom_point(aes(x.excluded,y.excluded), colour = "gray")
    # x-y.not.excluded are the points to which the linear fit will be applied; 
    # x-y.fit differ in that they may be subsequently log-transformed (to apply cor.test):
    x.not.excluded[excluded.points] = x.fit[excluded.points] = NA
    y.not.excluded[excluded.points] = y.fit[excluded.points] = NA
  }
  
  if (x.log)
  {
    plot = plot + scale_x_log10()
    if (fit)
    {
      x.pos = (max(x,na.rm=T)/min(x,na.rm=T))^x.cor.pos*min(x,na.rm=T)
      x.fit = log10(x.fit)
      # x.fit[is.infinite(x.fit)] = NA
    }
    if (!is.null(letter))
      letter.x.pos = (max(x,na.rm=T)/min(x,na.rm=T))^position.letter.xy[1]*min(x,na.rm=T)
  } else
  {
    # plot = plot + xlim(range(x))
    if (fit)
    {
      x.pos = x.cor.pos*(max(x,na.rm=T)-min(x,na.rm=T))+min(x,na.rm=T)
    }
    if (!is.null(letter))
      letter.x.pos = position.letter.xy[1]*(max(x,na.rm=T)-min(x,na.rm=T))+min(x,na.rm=T)
  }
  
  if (y.log)
  {
    plot = plot + scale_y_log10()
    if (fit)
    {
      y.pos = (max(y,na.rm=T)/min(y,na.rm=T))^y.cor.pos*min(y,na.rm=T)
      y.fit = log10(y.fit)
      # y.fit[is.infinite(y.fit)] = NA
    }
    if (!is.null(letter))
      letter.y.pos = (max(y,na.rm=T)/min(y,na.rm=T))^position.letter.xy[2]*min(y,na.rm=T)
  } else
  {
    # plot = plot + ylim(range(y))
    if (fit)
    {
      y.pos = y.cor.pos*(max(y,na.rm=T)-min(y,na.rm=T))+min(y,na.rm=T)
    }
    if (!is.null(letter))
      letter.y.pos = position.letter.xy[2]*(max(y,na.rm=T)-min(y,na.rm=T))+min(y,na.rm=T)
  }
  
  if (fit)
  {
    cor.test = cor.test(x.fit,y.fit,na.rm=T)
    plot = plot + geom_smooth(aes(x.not.excluded,y.not.excluded),method='lm',col="black") +
      annotate(geom="text", 
               x=x.pos,
               y=y.pos,
               label=bquote(atop(rho==.(format(cor.test$estimate,digits=2,nsmall=2)),p==.(format(cor.test$p.value,digits=1)))),
               size=8)
  }
  
  if (!is.null(letter))
  {
    plot = plot + annotate(geom="text",
                           x = letter.x.pos,
                           y = letter.y.pos,
                           label=letter,
                           # label = bquote("Median"==.(format(median.adjR2.per.MEM[k],digits=2))),
                           hjust = 1,
                           size=12)
  }
  return(plot)
}
