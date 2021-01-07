# Linear regression plot:

cor.plot <- function(x,y,col.factor=NULL,col.vect=NULL,leg.name="Legend",
                     excluded.points=NULL,
                     x.lab,y.lab,y.lab.hjust=0.5,size.factor=1,
                     x.log=F,y.log=F,
                     bin.nb=NULL,
                     binning = "equal_bin_size", # two options: "equal_spacing" or "equal_bin_size"
                     fit=T,x.cor.pos=0.8,y.cor.pos=0.1,dis=F,signif.thres=NULL,
                     mar.vect=c(5,5,5,5),letter=NULL,position.letter.xy=c(0.1,0.9),letter.size=12)
{
  # dis = T is only useful if fit=T, so that a Mantel test is performed rather than a t-test
  # dis = T cannot be combined with excluded.points!=NULL and x.log or y.log
  if (dis)
  {
    x.dis = as.dist(x)
    y.dis = as.dist(y)
    x = x[lower.tri(x)]
    y = y[lower.tri(y)]
  }
  
  x[is.infinite(x)] = NA
  y[is.infinite(y)] = NA
  
  if (x.log)
    x[x == 0] = NA
  if (y.log)
    y[y == 0] = NA
  
  if (is.null(col.factor))
  {
    plot = ggplot(data = data.frame(x = x, y = y)) + 
      geom_point(aes(x,y), colour = ifelse(is.null(bin.nb),"black","gray"), size = 1*size.factor)
  } else
    # requires library(colorspace):
    plot = ggplot(data = data.frame(x = x, y = y, col = col.factor)) + 
      geom_point(aes(x,y, colour = factor(col.factor)), size = 1*size.factor) +
      scale_colour_manual(values = if (is.null(col.vect)) rainbow_hcl(length(levels(factor(col.factor)))) else col.vect,
                          name = leg.name) +
      guides(colour = guide_legend(override.aes = list(size=3*size.factor)))
  
  plot = plot +
    # geom_point(aes(x,y1)) +
    # scale_x_log10() +
    # scale_y_log10() +
    theme_bw() +
    # ggtitle(LETTERS[6]) +
    # geom_hline(yintercept = 1, linetype="dashed") +
    # ggtitle("RDA adjR2 3 vs 2 abiotic PCA axes with stdzation") +
    # ggtitle("Pure abiotic vs. total abiotic adjR2 3 axes") +
    theme(axis.text = element_text(size=22*size.factor),
          axis.title=element_text(size=22*size.factor),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24*size.factor),
          plot.margin=unit(mar.vect,"mm")) +
    #labs(x="Mean size (micron)", y=paste("Ratio of",c("surface","DCM")[i_case],"variance\n purely explained by currents vs. envir.")) +
    labs(x=x.lab, y=y.lab)
  
  x.not.excluded = x
  y.not.excluded = y
  if (length(excluded.points)>0)
  {
    x.excluded = x
    y.excluded = y
    # Plotting in grey the points that are excluded from fitting:
    x.excluded[-excluded.points] = NA
    y.excluded[-excluded.points] = NA
    plot = plot +
      geom_point(aes(x.excluded,y.excluded), colour = ifelse(is.null(bin.nb),"gray","lightgray"))
    # x-y.not.excluded are the points to which the linear fit will be applied; 
    # x-y.fit differ in that they may be subsequently log-transformed (to apply cor.test):
    x.not.excluded[excluded.points] = NA
    y.not.excluded[excluded.points] = NA
  }
  
  # Binning, excluding excluded points: 
  if (!is.null(bin.nb))
  {
    if (binning == "equal_spacing")
    {
      bin.size = (max(x.not.excluded,na.rm=T)-min(x.not.excluded,na.rm=T))/bin.nb
      x.bin = min(x.not.excluded,na.rm=T) + 1:bin.nb*bin.size
      y.bin = vector(length = bin.nb, mode = "numeric")
      for (i in 1:bin.nb)
      {
        upper.bin = x.bin[i]  
        lower.bin = x.bin[i] - bin.size
        y.bin[i] = mean(y.not.excluded[x.not.excluded < upper.bin & x.not.excluded > lower.bin], na.rm=T)
      }
      # Centering x.bin for plotting: 
      x.bin = x.bin - bin.size/2
    } else if (binning == "equal_bin_size")
    {
      bin.size = length(x.not.excluded) %/% bin.nb
      y.not.excluded.sorted = y.not.excluded[sort.int(x.not.excluded,index.return = T)$ix]
      x.not.excluded.sorted = sort(x.not.excluded)
      y.bin = x.bin = vector(length = bin.nb, mode = "numeric")
      for (i in 1:(bin.nb-1))
      {
        y.bin[i] = mean(y.not.excluded.sorted[((i-1)*bin.size+1):(i*bin.size)], na.rm = T)
        x.bin[i] = mean(x.not.excluded.sorted[((i-1)*bin.size+1):(i*bin.size)], na.rm = T)
      }
      y.bin[bin.nb] = mean(y.not.excluded.sorted[((bin.nb-1)*bin.size+1):length(y.not.excluded)], na.rm = T)
      x.bin[bin.nb] = mean(x.not.excluded.sorted[((bin.nb-1)*bin.size+1):length(x.not.excluded)], na.rm = T)
    }
    # Updating the plot:
    plot = plot +
      geom_point(data = data.frame(x = x.bin, y = y.bin), aes(x,y), size = 1*size.factor)
  }
  
  # x.plot and y.plot are the points appearing on the fit plot (either x.not.excluded and y.not.excluded, or x.bin and y.bin)
  # x.fit and y.fit simply differ from x.plot and y.plot in that they may need to be log-transformed:
  if (is.null(bin.nb))
  {
    x.plot = x.not.excluded
    y.plot = y.not.excluded
    if (dis)
    {
      x.fit = x.dis
      y.fit = y.dis
    } else
    {
      x.fit = x.not.excluded
      y.fit = y.not.excluded
    }
  } else
  {
    x.fit = x.plot = x.bin 
    y.fit = y.plot = y.bin
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
    # If the data are dissimilarities, performing a Mantel test rather than a t-test (using Pearson's correlation in both cases).
    if (!dis || !is.null(bin.nb))
    {   
      cor.test = cor.test(x.fit,y.fit,na.rm=T)
      stat = cor.test$estimate
      pval = cor.test$p.value
    } else if (dis && is.null(bin.nb))
    {
      test = vegan::mantel(x.fit,y.fit,permutations = 999,na.rm=T)
      stat = test$statistic
      pval = test$signif
    }
    plot = plot + geom_smooth(data = data.frame(x = x.plot, y = y.plot), aes(x,y), method='lm',col="black", size = 1*size.factor) +
      annotate(geom="text", 
               x=x.pos,
               y=y.pos,
               label=bquote(atop(rho==.(format(stat,digits=2,nsmall=2)),p==.(format(pval,digits=1)))),
               col= if (!is.null(signif.thres)) ifelse(pval < signif.thres,"blue","black") else "black",   
               size=8*size.factor)
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
