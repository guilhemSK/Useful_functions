functional.boxplot <- function(classes, values, 
                               detached.points = NULL, excluded.points = NULL,
                               y.lab, y.lab.hjust=0.5, y.log=F,
                               mar.vect=c(5,5,5,5), letter = NULL, position.letter.xy = c(0.1,0.9))
{
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
    theme(axis.title=element_text(size=22),
          axis.text=element_text(size=22),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.margin=unit(mar.vect,"mm")) +
    labs(x="", y= y.lab)
  
  if (!is.null(detached.points) || !is.null(excluded.points))
    plot = plot + ylim(range(values))
  
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
                 aes(x,y), colour = "gray")
  }
  
  if (y.log)
  {
    plot = plot + scale_y_log10()
    if (!is.null(letter))
      letter.y.pos = (max(y,na.rm=T)/min(y,na.rm=T))^position.letter.xy[2]*min(y,na.rm=T)
  } else
  {
    if (!is.null(letter))
      letter.y.pos = position.letter.xy[2]*(max(y,na.rm=T)-min(y,na.rm=T))+min(y,na.rm=T)
  }
  
  if (!is.null(letter))
  {
    plot = plot + annotate(geom="text",
                           x = 0.5 + length(levels(classes))*position.letter.xy[1],
                           y = letter.y.pos,
                           label=letter,
                           # label = bquote("Median"==.(format(median.adjR2.per.MEM[k],digits=2))),
                           hjust = 1,
                           size=12)
  }
  
  return(plot)
}
