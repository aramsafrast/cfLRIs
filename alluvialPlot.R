alluvialPlot <- function(bsrinf, keywords, type=c("L","R","pw.id"),
                         qval.thres=0.01, format=c("pdf","svg","png"),
                         path="./", filename = NULL,
                         width = 10, height = 10) {
  
  interactions <- data.frame(
    L =  bsrinf$L,
    R =  bsrinf$R,
    pw.name = bsrinf$pw.name,
    pw.id = bsrinf$pw.id,
    qval = bsrinf$qval
  )
  
  type <- match.arg(type)
  format <- match.arg(format)
  
  if(type=="L")
    subset.interactions <- interactions[interactions$L %in% keywords,]
  if(type=="R")
    subset.interactions <- interactions[interactions$R %in% keywords,]
  if(type=="pw.id")
    subset.interactions <- interactions[interactions$pw.id %in% keywords,]  
  
  if (dim(subset.interactions)[1]==0){
    cat(paste(keywords, collapse = ' ')," for ", type, " not found.","\n", sep="")
    stop("Try another value for filtering.")
  }
  subset.interactions <- subset.interactions[subset.interactions$qval <= qval.thres,]  
  subset.interactions$count <- 1
  subset.interactions <- subset.interactions[,c("L" , "R", "pw.name" ,"count")]
  
  library(stringr)
  max_label_length <- 50  # Adjust this as per your need
  subset.interactions$pw.name <- str_wrap(subset.interactions$pw.name, width = max_label_length)
  
  stratum <- ggalluvial::StatStratum
  
  pl <- ggplot2::ggplot(subset.interactions,
                        ggplot2::aes_string(y = "count", axis1 = "L", axis2 = "R",axis3 = "pw.name")) +
    ggalluvial::geom_alluvium(ggplot2::aes_string(fill = "R"), width = 1/12) +
    ggalluvial::geom_stratum(width = 1/12, fill = "black", color = "grey") +
    ggplot2::geom_label(stat = stratum,  aes(label = ggplot2::after_stat(stratum))) +
    ggplot2::scale_x_discrete(limits = c("L", "R","pw.name"), expand = c(0.5, 0.5)) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
    ggplot2::ggtitle("Ligand-Receptor Interactions & Underlying Pathways")
  
  pl <- pl + ggplot2::theme_bw()
  pl <- pl + ggplot2::theme(legend.position = "none")
  pl <- pl + ggplot2::theme(axis.title = ggplot2::element_blank(),
                            text = ggplot2::element_text(size = 12),
                            plot.title = ggplot2::element_text(hjust = .5),
                            axis.text.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks = ggplot2::element_blank(),
                            panel.grid = ggplot2::element_blank())
  
  if (!is.null(filename)){
    if (format=="svg")
      grDevices::svg(file=paste0(path,filename,".svg"),
                     width=width/2.54, height=height/2.54) 
    
    if (format=="png")
      grDevices::png(file=paste0(path,filename,".png"),
                     width=width/2.54, height=height/2.54,units="in",res=600) 
    
    if (format=="pdf")
      grDevices::pdf(file=paste0(path,filename,".pdf"),
                     width=width/2.54, height=height/2.54)
  } 
  print(pl)
  if (!is.null(filename))
    grDevices::dev.off()
}