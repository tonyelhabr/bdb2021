
gg_field <- function(yardmin=0, yardmax=120, buffer=5, direction="horiz",
                     field_color='white',line_color='black',
                     sideline_color=field_color,endzone_color=field_color){

  ## field dimensions (units=yards)
  xmin <- 0
  xmax <- 120

  ymin <- 0
  ymax <- 53.33


  ## distance from sideline to hash marks in middle (70 feet, 9 inches)
  hash_dist <- (70*12+9)/36

  ## yard lines every 5 yards
  yd_lines <- seq(15,105,by=5)

  ## hash mark locations
  yd_hash <- 11:109

  ## number size
  num_size <- 5

  ## rotate field numbers with field direction
  ## first element is for right-side up numbers, second for upside-down
  angle_vec <- switch(direction, "horiz" = c(0, 180), "vert" = c(270, 90))
  num_adj <- switch(direction, "horiz" = c(-1, 1), "vert" = c(1, -1))

  ## list of annotated geoms
  p <- list(

    ## add grass (with buffer on sidelines in case players run OOB)
    ggplot2::annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin-buffer, ymax=ymax+buffer,
             fill=field_color),

    ## add end zones
    ggplot2::annotate("rect", xmin=xmin, xmax=xmin+10, ymin=ymin, ymax=ymax, fill=endzone_color),
    ggplot2::annotate("rect", xmin=xmax-10, xmax=xmax, ymin=ymin, ymax=ymax, fill=endzone_color),

    ## add yardlines every 5 yards
    ggplot2::annotate("segment", x=yd_lines, y=ymin, xend=yd_lines, yend=ymax,
             col=line_color),

    ## add thicker lines for endzones, midfield, and sidelines
    ggplot2::annotate("segment",x=c(0,10,60,110,120), y=ymin, xend=c(0,10,60,110,120), yend=ymax,
             lwd=1.3, col=line_color),
    ggplot2::annotate("segment",x=0, y=c(ymin, ymax), xend=120, yend=c(ymin, ymax),
             lwd=1.3, col=line_color) ,

    ## add numbers every 10 yards

    ggplot2::annotate("text",x=seq(20,100,by=10)+num_adj[2], y=ymin+12, label=0, angle=angle_vec[1],
             col=line_color, size=num_size),

    ## add all numbers separately
    # annotate("text",label=1,x=c(20,100)-1, y=ymin+12, angle=angle_vec[1],
    #          colour=line_color, size=num_size),
    ggplot2::annotate("text",label=1,x=c(20,100)+num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=2,x=c(30,90)+num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=3,x=c(40,80)+num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=4,x=c(50,70)+num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=5,x=60+num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),


    # ## upside-down numbers top of field
    # annotate("text",x=seq(20,100,by=10)+1, y=ymax-12, angle=180,
    #          label=c(1,2,3,4,5,4,3,2,1), col='white', cex=5),
    ggplot2::annotate("text",x=seq(20,100,by=10)+num_adj[1], y=ymax-12, angle=angle_vec[2],
             label=0, col=line_color, size=num_size),

    ## add all numbers separately
    ggplot2::annotate("text",label=1,x=c(20,100)+num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=2,x=c(30,90)+num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=3,x=c(40,80)+num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=4,x=c(50,70)+num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ggplot2::annotate("text",label=5,x=60+num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),


    # ## add tick marks every yard - middle of field
    ggplot2::annotate("segment", x=yd_hash, y=hash_dist - 0.5, xend=yd_hash, yend=hash_dist + 0.5,
             color=line_color),
    ggplot2::annotate("segment", x=yd_hash, y=ymax - hash_dist - 0.5,
             xend=yd_hash, yend=ymax - hash_dist + 0.5,color=line_color),

    # ## add tick marks every yard - sidelines
    ggplot2::annotate("segment", x=yd_hash, y=ymax, xend=yd_hash, yend=ymax-1,color=line_color),
    ggplot2::annotate("segment", x=yd_hash, y=ymin, xend=yd_hash, yend=ymin+1,color=line_color),

    ## add weird conversion lines at 2-yard line
    ggplot2::annotate("segment",x=12, y=(ymax-1)/2, xend=12, yend=(ymax+1)/2, color=line_color),
    ggplot2::annotate("segment",x=108, y=(ymax-1)/2, xend=108, yend=(ymax+1)/2, color=line_color),

    ## cover up lines outside of field
    ggplot2::annotate("rect", xmin=0, xmax=xmax, ymin=ymax, ymax=ymax+buffer, fill=sideline_color),
    ggplot2::annotate("rect",xmin=0, xmax=xmax, ymin=ymin-buffer, ymax=ymin, fill=sideline_color),

    ## remove axis labels and tick marks
    ggplot2::labs(x="", y=""),
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()),

    # clip view of field
    ggplot2::coord_cartesian(xlim=c(yardmin, yardmax),ylim = c(ymin-buffer,ymax+buffer), expand = FALSE),

    if(direction=="vert"){
      ggplot2::coord_flip(xlim=c(yardmin, yardmax),ylim = c(ymin-buffer,ymax+buffer), expand = FALSE)
    }
  )

  return(p)
}
