library(tidyverse)
library(extrafont)

cli::cli_h1("Function & objcets to edit your template")
cli::cli_li(
  c(
    "{.code .fig_counter}: # of plot",
    "{.code .tbl_counter}: # of tbls",
    "{.fn .gt_finalise}: use it at the end of a gt object with pipe",
    "{.code hsz_theme}: ggplot theme for hsz (set as defautl)",
    "{.code hsz_theme_w_vertical}: gg th for ps where vertical is meaningful",
    "{.code .co_pal}: A suggested color vector",
    "{.fn .co}: fn to easily access these colors",
    "{.fn .gg_finalise}: A fn to add header and others. ! Not pipe compatible."
  )
)

.fig_counter <- 1
.tbl_counter <- 1

# tables (gt) -------------------------------------------------------------

library(gt)

.gt_finalise <- function(.gt, ...) {
  
  subtitle <- knitr::opts_current$get()$`tbl-cap`
  subcap <- knitr::opts_current$get()$`tbl-subcap`
  
  if (is.null(subtitle)) {
    subtitle <- "A cím csak a dokumentum rendelésekor jelenik meg.A cím csak a dokumentum rendelésekor jelenik meg."
  }
  
  title <- str_c(.tbl_counter, ". táblázat")
  .tbl_counter <- .tbl_counter + 1
  
  .gt <- .gt |> 
    tab_style(
      style = list(
        cell_fill(color = "#e4e4e3"),
        cell_borders(sides = "all", color = "black", weight = px(.5)),
        cell_text(font = "Calibri", align = "center")
      ),
      locations = cells_body()
    ) |>
    tab_style(
      style = list(
        cell_fill(color = "#e4e4e3"),
        cell_borders(sides = "all", color = "black", weight = px(.5)),
        cell_text(font = "Calibri", align = "center", weight = "bold")
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_borders(sides = "left", style = "hidden"),
      locations = list(
        cells_column_labels(columns = 1), 
        cells_body(columns = 1)
      )
    ) |>
    tab_style(
      style = cell_borders(sides = "right", style = "hidden"),
      locations = list(
        cells_column_labels(columns = last_col()), 
        cells_body(columns = last_col())
      )
    ) |>
    tab_header(
      title = title,
      subtitle = subtitle
    ) |> 
    tab_style(
      style = list(
        cell_text(
          font = "Calibri",
          align = "left", 
          weight = "bold",
          color = "black"
        ),
        cell_borders(sides = "bottom", color = "#9f9f9e"),
        cell_borders(sides = "top", color = "black")
      ),
      locations = cells_title()
    ) |>
    tab_options(
      table.border.top.color = "black",
      table.border.top.width = px(4),
      table.border.bottom.color = "black",
      table.border.bottom.width = px(4),
      column_labels.border.top.color = "#9f9f9e",
      column_labels.border.bottom.style = 'none',
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "none",
      heading.border.bottom.style = "none",
      heading.align = 'left',
      heading.background.color = '#9f9f9e',
      heading.title.font.size = px(21),
      heading.subtitle.font.size = px(21),
      table.font.size = px(21),
      table.width = px(1000),
      footnotes.font.size = px(21)
    )
  
  if (!is.null(subcap)) {
    .gt <- .gt |> 
      tab_footnote(subcap) |> 
      tab_style(
        style = list(
          cell_text(
            font = "Calibri",
            align = "left",
            color = "black"
          ),
          cell_fill(color = "#e4e4e3")
        ),
        locations = cells_footnotes()
      )
  }
  
  if (!knitr::is_latex_output() &&
      !knitr::pandoc_to("docx") &&
      !knitr::pandoc_to("beamer") &&
      !knitr::is_html_output()) {
    .gt
  } else {
    label <- knitr::opts_current$get()$label
    
    if (is.null(label)) {
      file_name <- str_c(str_flatten(sample(c(letters, 0:9), 16)), ".png")
    } else {
      file_name <- str_c(label, ".png")
    }
    dir.create("gt", showWarnings = FALSE)
    gtExtras::gtsave_extra(.gt, expand = c(1000, 1000, 1000, 1000), filename = str_c("gt/", file_name))
    cat(str_c("![](gt/", file_name, ")"))
    
  }
}

# plots -------------------------------------------------------------------

# * HSZ theme
# font from: https://www.rmtweb.co.uk/calibri-and-cambria-fonts-for-mac
hsz_theme <- theme_classic(base_family = "Calibri", base_size = 10) + 
  theme(
    text = element_text(family = "Calibri", size = 0),
    plot.background = element_rect(fill = "#e4e4e3", color = "#e4e4e3"),
    panel.background = element_rect(fill = "#e4e4e3"),
    axis.ticks.length=unit(.15, "cm"),
    axis.ticks = element_line(linewidth = .8, color = "black"),
    legend.position = "bottom",
    legend.background = element_blank(),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0),
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(linetype = "dotted", color = "#969898"),
    axis.title = element_text(family = "Calibri", color = "black", size = 8),
    axis.line = element_line(linewidth = .8, color = "black"),
    strip.background = element_rect(fill = "#969898", linewidth = .8)
  )

hsz_theme_w_vertical <- hsz_theme + 
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "#969898"),
  )

theme_set(hsz_theme)

# suggested colors for HSZ

.co_pal <- tribble(
  ~ hex_code, ~ name,
  "#18223e", "blue",
  "#6fa0be", "blue2", 
  "#f8c567", "yellow",
  "#b2242a", "red",
  "#7aa140", "green",
  "#da3232", "red2",
  "#e57b2b", "orange",
  "#787975", "grey", 
  "#b9e1eb", "blue3"
) |> 
  pull(hex_code, name = name)

.co <- \(x) set_names(.co_pal[x], NULL)

update_geom_defaults("point", list(shape = 21, color = "black", size = 2))
update_geom_defaults("line", list(size = 1.1, color = .co("blue")))

`-.gg` <<- function(e1, e2) e2(e1)

.gg_finalise <- function(plot_name = ggplot2::last_plot())  {
  
  title <- knitr::opts_current$get()$`fig.cap`
  w <- knitr::opts_current$get()$`fig.width`
  h <- knitr::opts_current$get()$`fig.height`
  
  if (is.null(title)) title <- "A cím csak a dokumentum rendelésekor jelenik meg."
  if (is.null(w)) w <- 6
  if (is.null(h)) h <- 4
  
  title <- str_c(.fig_counter, ". ábra\n", str_wrap(title, 90))
  
  n_rows_title <- str_count(title, "\n") + 1.2
  height_of_footer <- (n_rows_title * 60)
  height_of_plot <- (h * 250 - 10) - height_of_footer
  
  .fig_counter <<- .fig_counter + 1
  
  borders_to_plot <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="black", col = "black"))
  )
  
  footer <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="#9f9f9e", col = "#9f9f9e"), ),
    grid::textGrob(title, x = 0.01, hjust = 0, y = .50, 
                   gp = grid::gpar(fontsize = 10.5, col = "black",
                                   fontfamily = "Calibri", fontface = "bold"))
  )
  
  pieces <- c("subtitle", "title", "caption")
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  plot_left_aligned <- grob
  
  label <- knitr::opts_current$get()$label
  if (is.null(label)) {
    file_name <- str_c(str_flatten(sample(c(letters, 0:9), 16)), ".png")
  } else {
    file_name <- str_c(label, ".png")
  }
  dir.create("figures", showWarnings = FALSE)
  
  gga <- ggpubr::ggarrange(
    borders_to_plot, footer, plot_left_aligned, borders_to_plot,
    ncol = 1,
    heights = c(5, height_of_footer, height_of_plot, 5)
  ) 
  
  ggpubr::ggexport(
    gga,
    filename = str_c("figures/", file_name),
    width = w * 250,
    height = h * 250, 
    res = 250
  )
  
  if (!knitr::is_latex_output() &&
      !knitr::pandoc_to("docx") &&
      !knitr::pandoc_to("beamer") &&
      !knitr::is_html_output()) {
    gga
  } else {
    # a new class with printing method `cat()`
    # This works properly with asis output thus makes the fn pipe compatible
    setClass("asistext", representation(value = "character"))
    setMethod("show", "asistext", \(object) cat(object@value))
    
    file_name |> # print the md code to read the figure
      str_c("![](figures/", `...` = _, ")") |> 
      as.character() |> 
      new("asistext", value = _)
  }
}

