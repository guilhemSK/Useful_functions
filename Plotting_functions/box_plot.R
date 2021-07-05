# box.plot takes a factor of classes and a vector of values as an input.
# The optional logical vector "detached.points" allows
# detach points from the box-plot representation
# The optional logical vector "excluded.points" allows
# plotting detached points in grey instead of black
box.plot <- function(classes, values, 
                     detached.points = NULL, excluded.points = NULL,
                     x.lab = NULL, y.lab, y.lab.hjust=0.5, size.factor=1, 
                     y.log=F,
                     y.lim = NULL,
                     x.angle = 45,
                     fit = F,fit.display = "anova",
                     fit.size.factor = 1,
                     x.cor.pos=0.8,y.cor.pos=0.8,
                     mar.vect=c(5,5,5,5), letter = NULL, position.letter.xy = c(0.1,0.9))
{
  classes = as.factor(classes)
  
  if (!is.null(detached.points) || !is.null(excluded.points))
  {
    classes.reduced = classes[-c(detached.points,excluded.points)]
    values.reduced = values[-c(detached.points,excluded.points)]
  } else
  {
    classes.reduced = classes
    values.reduced = values
  }
  
  plot = ggplot(data = data.frame(x = classes.reduced,
                                  y = values.reduced)) +
    scale_x_discrete(limits=levels(classes)) +
    geom_boxplot(aes(x,y)) +
    # scale_y_log10() +
    # geom_point(data = data.frame(x = factor(point_groups),
    #                              y = NormalizedVI_pcoa[[3]][,1]
    #                              [dominant_function0 %in% point_groups]),
    # aes(x,y)) +
    theme_bw() +
    # ggtitle(LETTERS[8]) +
    # geom_hline(yintercept = 1, linetype = "dashed") +
    theme(axis.title=element_text(size=22*size.factor),
          axis.text=element_text(size=22*size.factor),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24*size.factor),
          axis.text.x = element_text(angle = x.angle, vjust = 1, hjust = if (x.angle == 45) 1 else 0.5),
          plot.margin=unit(mar.vect,"mm")) +
    labs(x=x.lab, y= y.lab) +
    scale_y_continuous(limits = if (is.null(y.lim)) range(values) else y.lim,
                       trans = if (y.log) "log10" else "identity")
  
  if (length(detached.points)>0)
  {
    plot = plot +
      geom_point(data = data.frame(x = classes[detached.points],
                                   y = values[detached.points]),
                 aes(x,y))
  }
  
  if (length(excluded.points)>0)
  {
    plot = plot +
      geom_point(data = data.frame(x = classes[excluded.points],
                                   y = values[excluded.points]),
                 aes(x,y), colour = "gray70")
  }
  
  if (fit && !is.null(excluded.points))
  {
    values.fit = values[-c(excluded.points)]
    classes.fit = classes[-c(excluded.points)]
  } else
  {
    values.fit = values
    classes.fit = classes
  }
  
  if (y.log)
  {
    if (fit)
    {
      y.pos = (max(values,na.rm=T)/min(values,na.rm=T))^y.cor.pos*min(values,na.rm=T)
      values.fit = log10(values.fit)
    }
    if (!is.null(letter))
      letter.y.pos = (max(values,na.rm=T)/min(values,na.rm=T))^position.letter.xy[2]*min(values,na.rm=T)
  } else
  {
    if (fit)
    {
      y.pos = y.cor.pos*(max(values,na.rm=T)-min(values,na.rm=T))+min(values,na.rm=T)
    }
    if (!is.null(letter))
      letter.y.pos = position.letter.xy[2]*(max(values,na.rm=T)-min(values,na.rm=T))+min(values,na.rm=T)
  }
  
  if (fit)
  {
    if (fit.display == "anova")
    {
      p.val = anova(lm(values.fit ~ classes.fit))$'Pr(>F)'
    }
    fit.label = bquote(atop("ANOVA:",P==.(format(p.val,digits=1))))
    
    plot = plot + annotate(geom="text", 
                           x = 0.5 + length(levels(classes))*x.cor.pos,
                           y = y.pos,
                           label=fit.label,
                           # col= if (!is.null(signif.thres)) ifelse(pval < signif.thres,"blue","black") else "black",   
                           size= 7.5*size.factor*fit.size.factor)
  }
  
  if (!is.null(letter))
  {
    plot = plot + annotate(geom="text",
                           x = 0.5 + length(levels(classes))*position.letter.xy[1],
                           y = letter.y.pos,
                           label=letter,
                           # label = bquote("Median"==.(format(median.adjR2.per.MEM[k],digits=2))),
                           hjust = 1,
                           size=12*size.factor)
  }
  
  return(plot)
}
