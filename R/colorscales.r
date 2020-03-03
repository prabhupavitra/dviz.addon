#' Customized set of base colors to be initialized in pprabhu_colors vector
pprabhu_colors <- c(
`red`        = "#e41a1c",
`green`      = "#4daf4a",
`lightgreen` = "#b2df8a",
`darkgreen`  = "#33a02c",
`blue`       = "#377eb8",
`lightblue`  = "#a6cee3",
`darkblue`   = "#1f78b4",
`lightorange`= "#fdbf6f",
`orange`     = "#ff7f00",
`yellow`     = "#ffff33",
`light grey` = "#cccccc",
`dark grey`  = "#666666",
`purple`     = "#984ea3",
`lightpurple`= "#cab2d6",
`darkpurple` = "#6a3d9a",
`brown`      = "#a65628",
`pink`       = "#f781bf",
`grey`       = "#999999",
`cream`      = "#ffff99",
`darkbrown`  = "#b15928",
`blues`      = c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#08519c","#2171b5","#08306b"),
`YlGnBu`     = c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"),
`RdBu`       = c("#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac"))
#'
#' @title Returns the hex code of the colors passed if available
#'
#' @description Function to extract colors from pprabhu_colors as hex codes
#'
#' @param ... Character names of pprabhu_colors
#'
#' @examples
#' pprabhu_colcode("pink","grey") # Returns hex codes of pink and grey
#' pprabhu_colcode() # Returns all colors available
#'
#' @export
pprabhu_colcode <- function(...) {
cols <- c(...)

if (is.null(cols))
    return (pprabhu_colors)

pprabhu_colors[cols]
}
#'
#'@title Customized color palettes for presentations
#'
#'@description
#' A collection of colour palettes for data science projects for presentations.
#' main : basic color palette
#' cool : color palette with cool colors
#' hot : color palette with warmer colors
#' div : color palette that puts equal emphasis on mid-range critical values and extremes at both ends of the data range.
#' sseq : single sequence palette that are suited to ordered data that progress from low to high.
#' mseq : multiple sequence palette that are suited to ordered data that progress from low to high.
#' qual : color palette that creates the primary visual differences between classes.
#'
#'@export
pprabhu_palettes <-
list(
# Main color palette used if none specified
`main`  = pprabhu_colcode("lightblue","darkblue", "lightgreen","darkgreen","pink","red","lightorange","orange","lightpurple","darkpurple","cream","darkbrown"),

`cool`  = pprabhu_colcode("lightblue","darkblue", "lightgreen","darkgreen","pink"),

`hot`   = pprabhu_colcode("yellow", "orange", "red"),

`div`   = pprabhu_colcode("RdBu1","RdBu2","RdBu3","RdBu4","RdBu5","RdBu6","RdBu7","RdBu8","RdBu9"),

`sseq`  = pprabhu_colcode("blues1","blues2","blues3","blues4","blues5","blues6","blues7","blues8","blues9"),

`mseq`  = pprabhu_colcode("YlGnBu1","YlGnBu2","YlGnBu3","YlGnBu4","YlGnBu5","YlGnBu6","YlGnBu7","YlGnBu8","YlGnBu9"),

`qual`  = pprabhu_colcode("red","blue","green","purple","orange","yellow","brown","pink","grey")
)
#'
#' @title Convert chosen palette to a color ramp
#'
#' @description Extends a hand designed Color Palette from pprabhu_palettes into continous color ramps
#'
#' @param palette Choose a palette from pprabhu_palettes
#'
#' @param reverse If reverse is FALSE, reverse the chosen palette
#'
#' @examples
#' library(scales)
#' show_col(pprabhu_pal("div")(10))
#'
#' @export
pprabhu_pal <- function(palette = "main", reverse = TRUE, ...) {
  pal <- pprabhu_palettes[[palette]]
  if (reverse){
    pal <- rev(pal)
  }
  # This function is useful for converting hand-designed `sequential' or `diverging' color schemes into continous color ramps
  return(colorRampPalette(pal, ...))
}
#'
#' @title Customized color aesthetics for your plot
#'
#' @description Applies your custom color scales as color aesthetics
#'
#' @rdname scale_color_pprabhu
#'
#' @param palette Choose from 'pprabhu_palettes' list
#'
#' @param reverse logical input, Do you want to reverse the order of colors?
#'
#' @param discrete whether to use a discrete/continuous colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' #' library(ggplot2)
#' # Create some variables
#' x <- 1:9 * 2
#' y <- 1:9 * 1
#' xy <- data.frame(x,y)
#' # Plots a scatter plot and applies "div" palette to color aesthetics
#' ggplot(xy,aes(x=x,y=y,color=as.character(y))) +
#'   geom_point(stat="identity",size=9,fill="white") +
#'   scale_color_pprabhu(palette = "hot")
#'
#' @export
scale_color_pprabhu <- function(..., palette="main",
                                discrete = TRUE, reverse = TRUE) {

  input_pal <- pprabhu_pal(palette,reverse = reverse, ...)

  if (discrete) {
    discrete_scale("colour", paste0("pprabhu_",palette), palette=input_pal,...)
  }
  else {
    scale_color_gradientn(colours = input_pal(256),...)
  }
}
#'
#' @title Scales for ggplot2 : scale_fill function
#'
#' @description Applies your custom color scales as fill aesthetics
#'
#' @rdname scale_fill_pprabhu
#'
#' @param palette Choose from 'pprabhu_palettes' list
#'
#' @param reverse logical input, Do you want to reverse the order of colors?
#'
#' @param discrete whether to use a discrete/continuous colour palette
#'
#' @param ... additional arguments to pass to scale_fill_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual
#'
#' @examples
#' library(ggplot2)
#' # Create some variables
#' x <- 1:9 * 2
#' y <- 1:9 * 1
#' xy <- data.frame(x,y)
#'
#' # Plots a histogram with discrete fill using "div" palette
#' ggplot(xy,aes(x=x,y=y,fill=as.character(y))) +
#'   geom_bar(stat="identity") +
#'   scale_fill_pprabhu(palette = "div")
#'
#'# Plots a histogram with continious fill using "hot" palette
#' ggplot(xy, aes(x = x)) +
#'      geom_histogram(aes(fill = ..count..), binwidth = 5) +
#'      scale_fill_pprabhu(palette = "hot",discrete=FALSE)
#'
#' @export
scale_fill_pprabhu <- function(..., palette="main",
                               discrete = TRUE, reverse = TRUE) {

  input_pal <- pprabhu_pal(palette,reverse = reverse, ...)

  if (discrete) {
    discrete_scale("fill", paste0("pprabhu_",palette), palette=input_pal,...)
  }
  else {
    scale_fill_gradientn(colours = input_pal(256),...)
  }
}
