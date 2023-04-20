mat.plot <- function(matrix,
                     plot.title = NA,
                     x.lab = NA, y.lab = NA, y.lab.hjust=0.5, size.factor=1,
                     axis.text = T,
                     leg.name="Legend",
                     high.col = "red",
                     low.col = NA,
                     mar.vect=c(5,5,5,5))
{
  melted.matrix = melt(matrix)
  
  plot = ggplot(melted.matrix, aes(x = Var2, y = Var1))
  
  if (max(matrix, na.rm =T) != min(matrix, na.rm =T))
  {
    if (is.na(low.col))
    {
      plot = plot + 
        geom_raster(aes(fill=value)) +
        scale_fill_gradient(low = "grey90", high = high.col, na.value = 'white',
                            name = if (!is.na(leg.name)) leg.name else "",
                            guide = if (!is.na(leg.name)) "colourbar" else F)
    } else
      plot = plot + 
        geom_raster(aes(fill=value)) +
        scale_fill_gradient2(low = low.col, mid = "grey90", high = high.col,
                             midpoint = 0,
                             na.value = 'white',
                             name = if (!is.na(leg.name)) leg.name else "",
                             guide = if (!is.na(leg.name)) "colourbar" else F)
  } else
    plot = plot + 
      geom_raster(aes(fill=as.factor(value))) +
      scale_fill_manual(values = "grey90", breaks = 0, na.value = 'white',
                        name = if (!is.na(leg.name)) leg.name else "",
                        guide = if (!is.na(leg.name)) "colourbar" else F)
  
  plot = plot + 
    theme_bw() +
    theme(axis.text = if (axis.text) element_text(size=24*size.factor) else element_blank(),
          axis.title.x = if (!is.na(x.lab)) element_text(size=24*size.factor) else element_blank(),
          axis.title.y = if (!is.na(y.lab)) element_text(size=24*size.factor, hjust = y.lab.hjust) else element_blank(),
          plot.title = if (!is.na(plot.title)) element_text(hjust=0, size=24*size.factor) else element_blank(),
          legend.title = if (!is.na(leg.name)) element_text(size=24*size.factor) else element_blank(),
          legend.text = if (!is.na(leg.name)) element_text(size=24*size.factor) else element_blank(),
          plot.margin = unit(mar.vect,"mm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x=x.lab, y=y.lab) +
    ggtitle(plot.title)
  
  if (!axis.text)
    plot = plot + theme(axis.ticks = element_blank())
  
    # theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
    #                    axis.text.y=element_text(size=9),
    #                    plot.title=element_text(size=11))
  
  return(plot) 
}

