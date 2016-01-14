#' Publication-ready ggplot2 Theme
#' @description My personal ggplot2 theme.
#' @param base_size Base font size. Defaults to 10.
#' @param base_family Defaults to default.
#' @export

KEStheme <- function(base_size = 10, base_family = "") {
	theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme( 
        axis.line =         element_blank(),
        axis.text.x =       element_text(size = base_size * 1.3, 
							lineheight = 1, colour = "black", vjust = 1,
							face="bold"),
        axis.text.y =       element_text(size = base_size * 1.3, 
							lineheight = 1.3, colour = "black", hjust = 1, 
							face="bold"),
        axis.ticks =        element_line(colour = "black"),
        axis.title.x =      element_text(size = base_size * 1.9, face="bold"),
        axis.title.y =      element_text(size = base_size * 1.9, angle = 90, 
							face="bold"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks.margin = unit(0.1, "cm"),

        legend.background = element_rect(colour="white"),
        legend.key =        element_rect(fill = "white", colour = "white"),
        legend.key.size =   unit(1.7, "lines"),
        legend.text =       element_text(size = base_size * 2),
        legend.title =      element_text(size = base_size * 2, face = "bold", 
							hjust = 0),
        legend.position =   "right",
        panel.background =  element_rect(fill = "white", colour = NA),
        panel.border =      element_rect(fill=NA, colour="black", size=1.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        panel.margin =      unit(0.25, "lines"),
        strip.background =  element_rect(fill = "white", colour = NA),
        strip.text.y =      element_text(size = base_size * 2, angle = -90),
        plot.background =   element_rect(fill = NA, colour = NA),
        plot.title =        element_text(size = base_size * 3),
        plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
	)
}
