## custom version of hte bbsBayes2 plot map function
##
##

#' Generate a map of trends by strata
#'
#' `plot_map()` allows you to generate a colour-coded map of the percent
#' change in species trends for each strata.
#'
#' @param slope Logical. Whether or not to map values of the alternative trend
#'   metric (slope of a log-linear regression) if `slope = TRUE` was used in
#'   `generate_trends()`,  through the annual indices. Default `FALSE`.
#' @param title Logical. Whether or not to include a title with species. Default
#'   `TRUE`.
#' @param alternate_column Character, Optional name of numerical column in
#'   trends dataframe to plot. If one of the columns with "trend" in the title,
#'   (e.g., trend_q_0.05 then the colour scheme and breaks will match those
#'   used in the default trend maps)
#' @param strata_custom (`sf`) Data Frame. Data frame
#'   of modified existing stratification, or a `sf` spatial data frame with
#'   polygons defining the custom stratifications. See details on strata_custom
#'   in `stratify()`.
#'
#' @family indices and trends functions
#'
#' @return a ggplot2 plot
#'

plot_map_custom <- function(trends,
                     slope = FALSE,
                     title = TRUE,
                     species = "",
                     alternate_column = NULL,
                     col_viridis = FALSE,
                     region_type = "stratum",
                     map = NULL,
                     facet_byr = NULL,
                     facet_byc = NULL,
                     overlay_map = NULL){

  # Checks

  alternate_trend_column <- NULL

  if(!is.null(facet_byr)){
  #lookup <- c("fbr" = facet_byr, "fbc" = facet_byc)
  trends[,"fbr"] <- trends[,facet_byr]
  }
  if(!is.null(facet_byc)){
    #lookup <- c("fbr" = facet_byr, "fbc" = facet_byc)
    trends[,"fbc"] <- trends[,facet_byc]
  }

  trends <- dplyr::filter(trends, .data$region_type == region_type)

  start_year <- min(trends$start_year)
  end_year <- min(trends$end_year)
  if(is.null(map)){
    map <- bbsBayes2::load_map("latlong")
  }


  if(!is.null(alternate_column)){
    if(!alternate_column %in% names(trends)){
      stop("alternate_column is missing from trends.",
           "Specify a numerical column from the trends dataframe")
    }
    ## if the values in the alternate column are trends, then use the same
    ## colour scheme and breaks to match the defaul trend maps
    if(grepl("trend",alternate_column,ignore.case = TRUE)){
      alternate_trend_column <- alternate_column
      alternate_column <- NULL
    }
  }

  if(is.null(alternate_column)){
    breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
    labls <- c(paste0("< ", breaks[1]),
               paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]),
               paste0("> ",breaks[length(breaks)]))
    labls <- paste0(labls, " %")


    if(is.null(alternate_trend_column)){
      if(slope){ trend_col <- "slope_trend" }else{trend_col <- "trend"}
    }else{
      trend_col <- alternate_trend_column
    }

    trends$t_plot <- as.numeric(as.character(trends[[trend_col]]))
    trends$t_plot <- cut(trends$t_plot, breaks = c(-Inf, breaks, Inf),
                         labels = labls)

    if(title) {
      title <- paste(species, trend_col, start_year, "-", end_year)
    } else title <- NULL



    map_plot <- dplyr::inner_join(x = map, y = trends, by = c("strata_name" = "region"))

    bb <- sf::st_bbox(map_plot)


    m <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map,
                       colour = "grey70", size = 0.1,
                       fill = NA)  +
      ggplot2::geom_sf(data = map_plot, ggplot2::aes(fill = .data$t_plot),
                       colour = "grey70", size = 0.1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = title,
                    fill = paste0(trend_col,"\n", start_year, "-", end_year)) +
      ggplot2::theme(legend.position = "right",
                     line = ggplot2::element_line(linewidth = 0.4),
                     rect = ggplot2::element_rect(linewidth = 0.1),
                     axis.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
    if(!is.null(overlay_map)){
      overlay_map <- sf::st_transform(overlay_map,sf::st_crs(map_plot))
      m <- m+ggplot2::geom_sf(data = overlay_map, fill = NA)
    }
    m <- m + ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                               ylim = bb[c("ymin","ymax")])

    if(!col_viridis) {
      # pal <- stats::setNames(
        # c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
        #   "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
      #   levels(map$t_plot))
      pal <- stats::setNames(
        c("#7E1700","#974D13", "#AC7726", "#C1A443", "#D2D384", "#C0E9C2", "#88D9D7",
          "#4BB2CE","#2C87BE", "#1E5FAC", "#023198"),
        # scico::scico(n = 11,
        #              palette = "roma"),
        levels(map_plot$t_plot)
      )

      m <- m + ggplot2::scale_fill_manual(values = pal,
                                          na.value = "white")
    } else {
      m <- m + ggplot2::scale_fill_viridis_d()
    }

    if(!is.null(facet_byr) & !is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(rows = ggplot2::vars(fbr),
                                   cols = ggplot2::vars(fbc))
    }
    if(!is.null(facet_byr) & is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(rows = ggplot2::vars(fbr))
    }
    if(is.null(facet_byr) & !is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(cols = ggplot2::vars(fbc))
    }

  }else{ # if plotting alternate_column
    trend_col <- alternate_column
    trends$t_plot <- as.numeric(as.character(trends[[trend_col]]))
    if(title) {
      title <- paste(species, alternate_column, start_year, "-", end_year)
    } else title <- NULL

    map_plot <- dplyr::inner_join(x = map, y = trends, by = c("strata_name" = "region"))

    bb <- sf::st_bbox(map_plot)

    m <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map,
                       colour = "grey70", size = 0.1,
                       fill = NA)  +
      ggplot2::geom_sf(data = map_plot, ggplot2::aes(fill = .data$t_plot),
                       colour = "grey70", size = 0.1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = title,
                    fill = paste0(trend_col,"\n", start_year, "-", end_year)) +
      ggplot2::theme(legend.position = "right",
                     line = ggplot2::element_line(linewidth = 0.4),
                     rect = ggplot2::element_rect(linewidth = 0.1),
                     axis.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
    if(!is.null(overlay_map)){
      overlay_map <- sf::st_transform(overlay_map,sf::st_crs(map_plot))
      m <- m+ggplot2::geom_sf(data = overlay_map, fill = NA)
    }
    m <- m + ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                        ylim = bb[c("ymin","ymax")])

    m <- m + ggplot2::scale_fill_viridis_c()


    if(!is.null(facet_byr) & !is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(rows = ggplot2::vars(fbr),
                                   cols = ggplot2::vars(fbc))
    }
    if(!is.null(facet_byr) & is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(rows = ggplot2::vars(fbr))
    }
    if(is.null(facet_byr) & !is.null(facet_byc)){
      m <- m + ggplot2::facet_grid(cols = ggplot2::vars(fbc))
    }

  }
  return(m)
}

