makeFootnote <- function(footnoteText=
  format(Sys.time(), "%d %b %Y"),
                         size= .9, color= gray(0.5),height)
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(0,"npc"),
            y= unit(height, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


makenote <- function(noteText=
  format(Sys.time(), "%d %b %Y"),
                         size= 1.5, color= "black",location, height)
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= noteText ,
            x = unit(location,"npc"),
            y= unit(height, "npc"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}
