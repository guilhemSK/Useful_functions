bar.plot <- function(barplot_data.frame, y.lab, y.lab.hjust=0.5, colors, bar.width, legend.position,
                     mar.vect=c(5,5,5,5), x.text = NULL, size.x.text = 16, angle.x.text = 90, vjust.x.text=0.5, hjust.x.text = 0,
                     letter = NULL, position.letter.xy = c(0.1,0.9))
{
  plot = ggplot(data = reshape2::melt(barplot_data.frame)) +
    geom_bar(aes(x = Var2, y = value, fill = Var1), width = bar.width, stat="identity") +
    theme_minimal() +
    theme(axis.text = element_text(size=22),
          axis.ticks.x = element_blank(),
          axis.title=element_text(size=22),
          axis.title.y = element_text(hjust = y.lab.hjust),
          plot.title=element_text(hjust=0, size=24),
          plot.margin=unit(mar.vect,"mm"),
          legend.position=legend.position,
          legend.text=element_text(size=20),
          legend.title=element_text(size=16, hjust = 10),
          legend.key = element_rect(size = 2, fill = "white", colour = "white")) +
    labs(x = "", y = y.lab) +
    scale_fill_manual(values=colors) +
    guides(fill = guide_legend(title = NULL, direction = "vertical"))
  
  if (!is.null(x.text))
  {
    plot = plot + 
      theme(axis.text.x = element_text(size=size.x.text,
                                       angle=angle.x.text,
                                       vjust=vjust.x.text,
                                       hjust=hjust.x.text)) +
      scale_x_discrete(labels = x.text)
  } else
    plot = plot + theme(axis.text.x = element_blank())
  
  if (!is.null(letter))
  {
    plot = plot + annotate(geom="text",
                           x = (max(x)-min(x))*position.letter.xy[1] + min(x),
                           y = (max(y)-min(y))*position.letter.xy[2] + min(y),
                           label=letter,
                           # label = bquote("Median"==.(format(median.adjR2.per.MEM[k],digits=2))),
                           hjust = 1,
                           size=9)
  }
  
  return(plot)
}