
library(tidyverse)
# library(ggdag)
library(bdb2021)
theme_set_and_update_bdb()
# theme_set(ggdag::theme_dag())

.tidy_dag <- function(ggdag) {
  ggdag %>% 
    ggdag::tidy_dagitty() %>%
    ggdag::node_status()
}

.node_labels <-
  c(
    'epa' = 'EPA',
    'has_same_defender' = 'Same Defender?',
    'is_target_picked' = 'Target Picked?',
    'player_tracking' = 'Player Tracking',
    'epa_predictors' = 'EPA Predictors'
  )

.get_valid_col_trt <- function() {
  c('is_target_picked', 'has_same_defender')
}

.validate_col_trt <- function(x = .get_valid_col_trt(), ...) {
  match.arg(x, ...)
}

.generate_simple_tidy_dag <- function(exposure = .get_valid_col_trt()) {
  exposure <- .validate_col_trt(exposure)
  fmla <- sprintf('epa ~ %s', exposure) %>% as.formula()
  ggdag::dagify(
    fmla,
    exposure = exposure,
    outcome = 'epa',
    labels = .node_labels
  ) %>%
    .tidy_dag()
}

.generate_tidy_dag <- function(..., exposure = .get_valid_col_trt()) {
  exposure <- .validate_col_trt(exposure, several.ok = TRUE)
  ggdag::dagify(
    epa ~ epa_predictors,
    epa ~ player_tracking,
    ...,
    exposure = exposure,
    outcome = 'epa',
    labels = .node_labels
  ) %>%
    .tidy_dag()
}

.save_dag <- function(data, col, suffix = NULL, ..., sep = '_') {
  status_colors <-
    c(
      exposure = 'cornflowerblue',
      outcome = 'darkorange',
      latent = 'grey50'
    )
  na_color <- 'grey20'
  viz <-
    data %>%
    ggplot() +
    aes(x = x, y = y, xend = xend, yend = yend) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point(aes(color = status)) +
    ggdag::geom_dag_label_repel(
      aes(label = label, fill = status),
      seed =  42,
      color = 'white',
      fontface = 'bold'
    ) +
    scale_color_manual(values = status_colors, na.value = na_color) +
    scale_fill_manual(values = status_colors, na.value = na_color) +
    guides(color = FALSE, fill = FALSE) +
    # ggdag::theme_dag() +
    theme(
      # panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank()
    ) +
    labs(
      x = NULL, y = NULL,
      ...
    )
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  ggsave(viz, filename = file.path(get_bdb_dir_figs(), sprintf('dag_%s%s.png', col, suffix)), type = 'cairo', width = 6, height = 6)
  viz
}

.save_simple_tidy_dag <- function(col, suffix = 't-test', col_pretty = NULL) {
  dag <- .generate_simple_tidy_dag(exposure = col)
  if(is.null(col_pretty)) {
    col_pretty <-
      case_when(
        col == 'is_target_picked' ~ 'Target Picked?',
        col == 'has_same_defender' ~ 'Same Defender?'
      )

    }  .save_dag(dag, col = col, suffix = suffix, title = sprintf('DAG for t-test for %s', col_pretty))
}

cols_trt <- .get_valid_col_trt()
walk(cols_trt, .save_simple_tidy_dag)

dag_is_target_picked <- .generate_tidy_dag(exposure = 'is_target_picked', epa ~ is_target_picked)
dag_has_same_defender <- .generate_tidy_dag(exposure = 'has_same_defender', epa ~ has_same_defender)

dag_hard <- 
  .generate_tidy_dag(
    epa ~ is_target_picked, 
    epa ~ has_same_defender ,
    has_same_defender ~ is_target_picked
  )

dag_is_target_picked %>%
  filter(name != 'player_tracking') %>%
  .save_dag(
    suffix = 'wo_player_tracking',
    title = 'DAG for estimating causal effect of\ntargeted pick route',
    subtitle = 'Without player tracking',
    col = 'is_target_picked'
  )

dag_is_target_picked %>%
  .save_dag(
    title = 'DAG for estimating causal effect of\ntargeted pick route',
    col = 'is_target_picked'
  )

dag_has_same_defender %>%
  .save_dag(
    title = 'DAG for estimating causal effect of\ntargeted defender coverage',
    col = 'has_same_defender'
  )

dag_hard %>%
  .save_dag(
    title = 'DAG for estimating causal effects of\ntargeted pick route and defender coverage',
    col = 'simultaneous'
  )


# # Unused stuff.
# dag_hard %>% ggdag::ggdag()
# dag_hard %>% ggdag::ggdag_paths(text = FALSE, use_labels = 'label', shadow = TRUE)
# dag_hard %>% ggdag::ggdag_adjustment_set(text = FALSE, use_labels = 'label', shadow = TRUE)
# dag_hard %>% ggdag::dag_paths()
# 
# library(ggdag)
# dag_has_same_defender %>%
#   dag_paths(paths_only = FALSE) %>%
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = path, alpha = path)) +
#   geom_dag_edges_link(
#     aes(
#       edge_alpha = path,
#       edge_colour = path,
#       start_cap = ggraph::circle(3, 'mm'),
#       end_cap = ggraph::circle(3, 'mm')
#     )
#   ) +
#   geom_dag_point(size = 4) +
#   facet_wrap(~forcats::fct_inorder(as.factor(set), ordered = TRUE)) +
#   scale_alpha_manual(
#     drop = FALSE,
#     values = c('open path' = 1),
#     na.value = .35,
#     breaks = 'open path'
#   ) +
#   ggraph::scale_edge_alpha_manual(
#     drop = FALSE,
#     values = c('open path' = 1),
#     na.value = .35,
#     breaks = 'open path'
#   ) +
#   ggraph::scale_edge_colour_hue(drop = FALSE, breaks = 'open path') +
#   scale_color_hue(drop = FALSE, breaks = 'open path') +
#   expand_plot(
#     expand_x = expansion(c(0.25, 0.25)),
#     expand_y = expansion(c(0.1, 0.1))
#   ) +
#   theme(legend.position = 'none')
# 
# dag_hard %>%
#   tidy_dagitty() %>%
#   dag_paths()
# dag_hard %>%
#   ggdag_paths()
