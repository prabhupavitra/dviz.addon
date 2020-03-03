
#' @title
#' Customized Themes for corporate level presentations
#'
#' @description
#' The base theme for this customized theme is cowplot. The default font for these themes
#' is Times, which needs to be installed on the target system for these themes to work.
#'
#' @param base_size Overall font size
#' @param base_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @examples
#' library(theme_pprabhu_dviz)
#'
#' @export
theme_pprabhu <- function (base_size = 14,
                           base_family = "Times",
                           line_size = .5,
                           rel_small = 12/14,
                           rel_tiny = 11/14,
                           rel_large = 16/14)
        {
        half_line <- base_size / 2
                cowplot::theme_half_open(font_size = base_size,
                                         font_family = base_family,
                                         line_size = line_size,
                                         rel_small = rel_small,
                                         rel_tiny = rel_tiny,
                                         rel_large = rel_large)  %+replace%
                theme(
                            plot.background = element_rect(fill="gray96", colour=NA),
                            plot.margin = margin(half_line/2, 1.5, half_line/2, 1.5),

                            legend.position = "bottom",

                            axis.text.x = element_text(size = base_size * 0.8,
                                                            angle = 330,
                                                            hjust = 0,
                                                            colour = "black"),
                            axis.text.y = element_text(size = base_size * 0.8,
                                                            angle = 0,
                                                            hjust = 1,
                                                            colour = "black"),

                            panel.background = element_rect(fill = "gray96",
                                                            colour = "gray96",
                                                            size = 0.5,
                                                            linetype = "solid"),
                            panel.grid.major = element_line(size = 0.3,
                                                            linetype = 'solid',
                                                            colour = "white"),
                            panel.grid.minor = element_line(size = 0.1,
                                                            linetype = 'solid',
                                                            colour = "white"),
                            complete = TRUE
            )
        }

#' @rdname theme_pprabhu
#'
#' @param colour Color for the grid lines
#'
#' @export
theme_pprabhu_grid <- function(base_size = 14,
                               base_family = "Times",
                               line_size = .5,
                               rel_small = 12/14,
                               rel_tiny = 11/14,
                               rel_large = 16/14,
                               colour = "grey90") {
half_line <- base_size / 2

cowplot::theme_minimal_grid(font_size = base_size,
                            font_family = base_family,
                            line_size = line_size,
                            rel_small = rel_small,
                            rel_tiny = rel_tiny,
                            rel_large = rel_large,
                            colour = colour)  %+replace%
theme(
    plot.background = element_rect(fill="gray96", colour=NA),
                        plot.margin = margin(half_line/2, 1.5, half_line/2, 1.5),

                        legend.position = "bottom",

                        axis.text.x = element_text(size = base_size * 0.8,
                                                        angle = 330,
                                                        hjust = 0,
                                                        colour = "black"),
                        axis.text.y = element_text(size = base_size * 0.8,
                                                        angle = 0,
                                                        hjust = 1,
                                                        colour = "black"),

                        panel.background = element_rect(fill = "gray96",
                                                        colour = "gray96",
                                                        size = 0.5,
                                                        linetype = "solid"),
                        panel.grid.major = element_line(size = 0.3,
                                                        linetype = 'solid',
                                                        colour = "white"),
                        panel.grid.minor = element_line(size = 0.1,
                                                        linetype = 'solid',
                                                        colour = "white"),
                        complete = TRUE
)
}

#' @rdname theme_pprabhu
#'
#' @param colour Color for the grid lines
#'
#' @export
theme_pprabhu_hgrid <- function(base_size = 14,
                                base_family = "Times" ,
                                line_size = .5,
                                rel_small = 12/14,
                                rel_tiny = 11/14,
                                rel_large = 16/14,
                                colour = "grey90") {
half_line <- base_size / 2

cowplot::theme_minimal_hgrid(font_size = base_size, font_family = base_family, line_size = line_size,
                            rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                            colour = colour)  %+replace%
theme(
    plot.background = element_rect(fill="gray96", colour=NA),
    plot.margin = margin(half_line/2, 1.5, half_line/2, 1.5),

    legend.position = "bottom",

    axis.text.x = element_text(size = base_size * 0.8,
                                angle = 330,
                                hjust = 0,
                                colour = "black"),
    axis.text.y = element_text(size = base_size * 0.8,
                                angle = 0,
                                hjust = 1,
                                colour = "black"),

    panel.background = element_rect(fill = "gray96",
                                colour = "gray96",
                                size = 0.5,
                                linetype = "solid"),
    panel.grid.major = element_line(size = 0.3,
                                linetype = 'solid',
                                colour = "white"),
    panel.grid.minor = element_line(size = 0.1,
                                linetype = 'solid',
                                colour = "white"),
    complete = TRUE
)
}

#' @rdname theme_pprabhu
#'
#' @param colour Color for the grid lines
#'
#' @export
theme_pprabhu_vgrid <- function(base_size = 14, base_family = "Times", line_size = .5,
                        rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                        colour = "grey90") {
half_line <- base_size / 2

cowplot::theme_minimal_vgrid(font_size = base_size, font_family = base_family, line_size = line_size,
                            rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                            colour = colour)  %+replace%
theme(
    plot.background = element_rect(fill="gray96", colour=NA),
    plot.margin = margin(half_line/2, 1.5, half_line/2, 1.5),

    legend.position = "bottom",

    axis.text.x = element_text(size = base_size * 0.8,
                                    angle = 330,
                                    hjust = 0,
                                    colour = "black"),
    axis.text.y = element_text(size = base_size * 0.8,
                                    angle = 0,
                                    hjust = 1,
                                    colour = "black"),

    panel.background = element_rect(fill = "gray96",
                                    colour = "gray96",
                                    size = 0.5,
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.3,
                                    linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.1,
                                    linetype = 'solid',
                                    colour = "white"),
    complete = TRUE
)
}

#' @rdname theme_pprabhu
#'
#' @param colour Color for the grid lines
#'
#' @export
theme_pprabhu_map <- function(base_size = 14, base_family = "Times", line_size = .5,
                        rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
half_line <- base_size / 2

cowplot::theme_map(font_size = base_size, font_family = base_family, line_size = line_size,
                        rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large)  %+replace%
theme(
    plot.background = element_rect(fill="gray96", colour=NA),
    plot.margin = margin(half_line/2, 1.5, half_line/2, 1.5),

    legend.position = "bottom",

    axis.text.x = element_text(size = base_size * 0.8,
                                    angle = 330,
                                    hjust = 0,
                                    colour = "black"),
    axis.text.y = element_text(size = base_size * 0.8,
                                    angle = 0,
                                    hjust = 1,
                                    colour = "black"),

    panel.background = element_rect(fill = "gray96",
                                    colour = "gray96",
                                    size = 0.5,
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.3,
                                    linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.1,
                                    linetype = 'solid',
                                    colour = "white"),
    complete = TRUE
)
}
