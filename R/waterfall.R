shift_on_ord <- function(x, n = 1L, type = c("lag", "lead")) {
  type <- match.arg(type)
  sux <- sort(unique(x))

  w <- match(x, sux) + n * c("lag" = -1L, "lead" = 1L)[type]
  w[w<=0] <- length(sux) + 1L
  sux[w]
}

seq_along_named <- function(x){
  y <- seq_along(x)
  names(y) <- as.character(x)

  return(y)
}


# from VHtools
diff_fill <- function(x, lag = 1L, differences = 1L, fill = NA) {
  if (isTRUE(length(x) > lag*differences)) {
    c(
      rep(methods::as(fill, typeof(x)), lag*differences), # fill initial missing values resulting from diff while preserving original type
      diff(x, lag = lag, differences = differences)
    )
  } else
    return(rep(as(NA, typeof(x)), length(x)))
}



#' Compute waterfall data
#' @param data a data.frame object
#'
#' @param detail_var name of the variable for item dimension
#' @param base_var name of the variable for totals
#' @param by_var name of the variable for facetting (optional)
#' @param value_var name of the variable with numeric values
#' @param drop_first drop observations for first base value (where differenced values are missing)
#' @param base_prefix prefix to be added to totals
#' @param ordering character vector with unique items (base_var) for manual ordering
#'
#' @usage
#' waterfall(
#'   data,
#'   detail_var = NULL,
#'   base_var   = NULL,
#'   by_var     = NULL,
#'   value_var  = NULL,
#'   drop_first = TRUE,
#'   base_prefix = "",
#'   ordering = NULL
#' )
#' @rdname waterfall_data
#' @export
waterfall <- function(data, ...) {
  UseMethod("waterfall", data)
}

#' @export
waterfall.data.table <- function(data, ...) {
  data <- copy(data)
  create_waterfall(data, ...)
}

#' @export
waterfall.data.frame <- function(data, ...) {
  create_waterfall(as.data.table(data), ...)
}

create_waterfall <- function(
  DT,
  detail_var = NULL,
  base_var   = NULL,
  by_var     = NULL,
  value_var  = NULL,
  drop_first = TRUE,
  base_prefix = "",
  ordering = NULL
) {

  # expand grid
  setkeyv(DT, c(by_var, base_var, detail_var))

  DT <-
    DT[
      if (length(by_var)) {
        CJ(
          get(by_var),
          get(base_var),
          get(detail_var),
          unique = TRUE)
      } else {
        CJ(
          get(base_var),
          get(detail_var),
          unique = TRUE)
      }
      ]
  DT[is.na(get(value_var)), c(value_var) := 0]

  # compute differences
  # setkeyv(DT, c(by_var, detail_var, base_var))
  DT[, value_diff := diff_fill(get(value_var)), by = c(by_var, detail_var)]

  if (is.null(ordering)) {
    ordering <- unique(DT[[detail_var]])
  }

  if (is.character(ordering)) ordering <- seq_along_named(ordering)

  DT[, ord := ordering[get(detail_var)]]
  setkeyv(DT, c(by_var, base_var, "ord"))

  # build waterfall data
  DT[, total := cumsum(value_diff),        by = c(by_var, base_var)]
  DT[, start := shift(total, 1, fill = 0), by = c(by_var, base_var)]
  DT[, end   := total,                     by = c(by_var, base_var)]

  # base values
  aggr <- DT[, .(value_aggr = sum(get(value_var))), by = c(by_var, base_var)]

  setkeyv(DT, c(base_var, by_var))
  DT[aggr[, .(shift_on_ord(get(base_var), type = "lead"), if (length(by_var)) get(by_var), value_aggr)], value_aggr := value_aggr]

  # shift waterfall values by base values
  DT[, start := start + value_aggr]
  DT[, end   := end   + value_aggr]

  # tag aggregate data
  aggr[, is_aggr := TRUE]
  DT[,   is_aggr := FALSE]

  # blend base data with waterfall data
  end_of_ord <- length(ordering) + 1
  aggr[, c("ord",      "start", "end",            "value_diff",     detail_var) :=
         .(end_of_ord,  0.0,     end = value_aggr, value_diff = NA, paste0(base_prefix, get(base_var)))]

  DT_OUT <-
    rbindlist(
      l = list(DT[,   c(base_var, by_var, "ord", "start", "end", "value_diff", detail_var, "is_aggr"), with = FALSE],
               aggr[, c(base_var, by_var, "ord", "start", "end", "value_diff", detail_var, "is_aggr"), with = FALSE]),
      use.names = TRUE,
      fill = TRUE)

  if (isTRUE(drop_first))
    DT_OUT <- DT_OUT[(get(base_var) != min(get(base_var))) | is_aggr == TRUE]

  setkeyv(DT_OUT, c(base_var, "ord"))
  DT_OUT[
    DT_OUT[,
           {cj <- CJ(get(base_var), ord, unique = TRUE);
           # setnames(cj, c(base_var, "ord"));
           cj[, I := .I];
           cj}],
    ordcol := I][]

  setattr(DT_OUT, "by_var",      by_var)
  setattr(DT_OUT, "detail_var",  detail_var)
  setattr(DT_OUT, "base_var", base_var)

  class(DT_OUT) <- c("waterfall", class(DT_OUT))

  return(DT_OUT)
}


#' @rdname waterfall_data
#' @export
waterfall_data <- waterfall



#' Plot waterfall data
#' @param x a data from waterfall() / waterfall_data()
#'
#' @param select apply a filter
#' @param scales passed to ggplot2
#' @param flip flip chart to horizontal bars?
#' @param xlim passed to ggplot2
#' @param ylim passed to ggplot2
#' @param nrow passed to ggplot2
#' @param ncol passed to ggplot2
#' @param label_font_size passed to ggplot2
#' @param color_palette (optional) manual color_palette
#' @param ... ignored
#'
#' @rdname waterfall_plot
#' @export
plot.waterfall <- function(
  x,
  select = TRUE,
  # scales = if (flip) "free_x" else "free_y",
  scales = "fixed",
  flip = FALSE,
  # units = 1,
  xlim = NULL,
  ylim = c(min(x$start, na.rm = TRUE), max(x$end, na.rm = TRUE)*1.1),
  nrow = NULL,
  ncol = NULL,
  label_font_size = 3,
  color_palette = NULL,
  ...
){

  sel_expr <- substitute(select)

  by_var      <- attr(x, "by_var")
  base_var    <- attr(x, "base_var")
  detail_var  <- attr(x, "detail_var")

  DT <- x[eval(sel_expr)]

  scale_table <- unique(DT[, c("ordcol", detail_var), with = FALSE])
  scale_vec        <- scale_table[[2]]
  names(scale_vec) <- scale_table[[1]]

  # breaks_fun <- function(lims) (ceiling(lims[1])+1):(floor(lims[2])-1)
  breaks     <- scale_table[[1]]
  labels_fun <- function(brks) scale_vec[as.character(brks)]

  # waterfall graf
  g <-
    ggplot(data = DT,
           aes(xmin = as.integer(ordcol) - 0.45,
               xmax = as.integer(ordcol) + 0.45,
               ymin = start,
               ymax = end,
               fill = factor(ifelse(is_aggr, "base", ifelse(value_diff > 0, "increase", "decrease")),
                             levels = c("decrease", "increase", "base")))) +
    geom_rect(color = "grey") +
    geom_text(aes(x = as.integer(ordcol),
                  y = pmax(start, end),
                  label = ifelse(is_aggr, "", paste0(ifelse(value_diff > 0, "+", ""), round(value_diff, digits = 2)))),
              size = label_font_size,
              hjust = if (flip) -0.2 else 0.5,
              vjust = if (flip) 0.5 else -0.3) +
    geom_text(aes(x = as.integer(ordcol),
                  y = pmax(start, end),
                  label = ifelse(!is_aggr, "", round(end, digits = 2))),
              size = label_font_size,
              hjust = if (flip) 1.2 else 0.5,
              vjust = if (flip) 0.5 else 1.5) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank())

  g <- g +
    if (length(color_palette))
      scale_fill_manual(guide = "none", values = color_palette) else
        scale_fill_manual(guide = "none", values = waterfall_colors)

  # use faceting if there is more than 1 dimension
  if (!is.null(by_var) & length(by_var) >= 1)
    g <- g +
    facet_wrap(by_var, scales = scales, nrow = nrow, ncol = ncol)


  # flip coordinates?
  if (isTRUE(flip)) {
    g <- g +
      coord_flip(
        xlim = xlim,
        ylim = ylim) +
      scale_x_reverse(
        breaks = breaks,
        # minor_breaks = NULL,
        labels = labels_fun) +
      theme(axis.ticks.y = element_blank())
  } else {
    g <- g +
      coord_cartesian(
        xlim = xlim,
        ylim = ylim) +
      scale_x_continuous(
        breaks = breaks,
        # minor_breaks = NULL,
        labels = labels_fun) +
      theme(axis.ticks.x = element_blank())
  }

  return(g)
}

#' @rdname waterfall_plot
#' @export
waterfall_plot <- plot.waterfall


#' color palette
#' @export
waterfall_colors <-
  c("base" = rgb(red = 155/255, green = 211/255, blue = 247/255),
    "decrease" = "#EA5B3F",
    "increase" = rgb(red = 146/255, green = 208/255, blue = 80/255))
