
# setup ----
library(tidyverse)
library(bdb2021)
theme_set_and_update_bdb()
data('receiver_intersections_adj', package = 'bdb2021')
data('plays_w_pick_off_info', package = 'bdb2021')
data('plays_w_pick_def_info', package = 'bdb2021')
data('plays_w_pick_info', package = 'bdb2021')
data('model_data_nflfastr', package = 'bdb2021')

plays <- import_plays()
pbp <- import_nflfastr_pbp()

# basic eda ----
viz_epa_swarm <-
  plays %>% 
  mutate(
    # across(is_pass_successful, binary_fct_to_lgl),
    across(is_pass_successful, ~if_else(.x == '1', 'Y', 'N')),
    across(
      epa, ~case_when(
        .x < -3 ~ -3,
        .x > 3 ~ 3,
        TRUE ~ .x
      )
    )
  ) %>% 
  ggplot() +
  aes(x = epa, y = is_pass_successful, color = is_pass_successful) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = FALSE, 
    varwidth = TRUE, 
    size = 1, 
    alpha = 0.2
  ) +
  scale_color_manual(values = c(`Y` = 'dodgerblue', `N` = 'darkorange')) +
  guides(
    color = guide_legend('Pass successful?', override.aes = list(alpha = 1, size = 3))
  ) +
  theme(
    legend.position = 'top',
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'EPA by pass success',
    caption = 'EPA truncated to -3 and 3.',
    x = 'EPA',
    y = NULL
  )
viz_epa_swarm
save_plot(viz_epa_swarm)

viz_intersections_after_n_sec <-
  receiver_intersections_adj %>% 
  semi_join(plays %>% select(game_id, play_id)) %>% 
  count(sec) %>% 
  mutate(
    across(sec, list(lab = ~sprintf('%1.1f < t <= %1.1f', sec - 0.5, sec))),
    across(sec_lab, ~forcats::fct_reorder(.x, sec))
  ) %>% 
  ggplot() +
  aes(x = sec_lab, y = n) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = 'Receiver Intersections n seconds after the ball snap',
    x = 'Seconds Into the Play',
    y = '# of Plays'
  )
viz_intersections_after_n_sec
save_plot(viz_intersections_after_n_sec)

viz_pick_play_frac <-
  plays_w_pick_off_info %>% 
  count(tm = possession_team, has_intersect) %>% 
  group_by(tm) %>% 
  mutate(
    frac = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(has_intersect == 1) %>% 
  mutate(
    rnk = row_number(-frac),
    across(tm, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  ggplot() +
  aes(x = frac, y = tm) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = '%s of plays involving at least one pick route combination',
    y = NULL,
    x = '% of Team\'s Plays'
  )
viz_pick_play_frac
save_plot(viz_pick_play_frac)

# causal analysis prep----
.str_replace_f <- function(x, i) {
  x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
}

.get_valid_col_trt <- function() {
  c('is_target_picked', 'has_same_defender')
}

.validate_col_trt <- function(x = .get_valid_col_trt(), ...) {
  match.arg(x, ...)
}

# dag stuff ----
if(FALSE) {
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
    
  }
  .save_dag(dag, col = col, suffix = suffix, title = sprintf('DAG for t-test for %s', col_pretty))
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
}

# causal stuff ----
.extract_x_from_fmla <- function(fmla) {
  # browser()
  fmla[[3]] %>% 
    as.character() %>% 
    .[2:length(.)] %>% 
    paste0(collapse = ' + ') %>% 
    str_replace_all(c('\\s[+*]\\s' = ', ')) %>% 
    # Remove leading +
    str_remove_all('[+]') %>% 
    str_trim() %>% 
    str_split(pattern = '\\, ') %>% 
    pluck(1) %>% 
    unique()
}

.aggregate_to_sd_diffs <- function(data, col_trt, fmla) {
  
  .validate_col_trt(col_trt)
  cols_features <- .extract_x_from_fmla(fmla)
  
  col_trt_sym <- col_trt %>% sym()
  agg <-
    data %>% 
    select(one_of(c(cols_features, col_trt))) %>% 
    mutate(
      across(where(is.double), ~if_else(.x == 9999, NA_real_, .x)),
      across(where(is.factor), as.integer),
      across(!!col_trt_sym, ~.x - 1L)
    ) %>% 
    group_by(!!col_trt_sym) %>% 
    summarise(
      across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      -one_of(col_trt)
    ) %>% 
    mutate(
      across(
        name,
        list(
          col = ~.str_replace_f(.x, 1),
          stat = ~.str_replace_f(.x, 2)
        ),
        .names = '{fn}'
      )
    ) %>% 
    select(-name)
  agg
  
  res <-
    agg %>% 
    pivot_wider(
      names_from = all_of(c('stat', col_trt)),
      values_from = 'value'
    ) %>% 
    mutate(sd_diff = (mean_1 - mean_0) / sd_1)
  res
}

do_causal_analysis <- function(col_trt = .get_valid_col_trt()) {
  # col_trt <- 'has_same_defender'
  .validate_col_trt(col_trt)

  fmla_chr <-
    sprintf('%s ~ x*y + x_o*dist_o + x_d*dist_d + x_o*x_d + dist_o*dist_d + half_seconds + yardline_100 + off_timeouts + def_timeouts + roof + is_home + down*yards_to_go', col_trt)
  # fmla_chr <- sprintf('%s ~ x', 'y')
  fmla <- fmla_chr %>% as.formula()
  
  fit <-
    glm(fmla, data = plays_w_pick_info, family = 'binomial')
  fit
  
  res_match <-
    Matching::Match(
      # caliper = 0.01,
      exact = TRUE,
      ties = FALSE,
      X = fit %>% fitted(),
      Y = plays_w_pick_info[['epa']],
      Tr = plays_w_pick_info[[col_trt]] %>% as.integer() %>% {. - 1L}
    )
  # # Lots of unnecesary stuff.
  # res_match
  # res_balance <- Matching::MatchBalance(fmla, data = plays_w_pick_info %>% mutate(across(col_trt, binary_fct_to_lgl)), match.out = res_match, nboots = 5)
  
  features_match <- 
    bind_rows(
      plays_w_pick_info[res_match[['index.control']], ] %>% mutate(grp = 'control'), 
      plays_w_pick_info[res_match[['index.treated']], ] %>% mutate(grp = 'treatment')
    )
  features_match
  
  if(col_trt == 'is_target_picked') {
    title_suffix_1 <- 'targeted pick play'
    title_suffix_2 <- 'targeted picks'
    ylim <- 100
    axis_label <- 'target picked'
  } else if (col_trt == 'has_same_defender') {
    title_suffix_1 <- 'targeted defender coverage'
    title_suffix_2 <- 'coverage'
    ylim <- 150
    axis_label <- 'same defender'
  }
  file_suffix <- col_trt
  
  .f_predict <- function(data) {
    fit %>% broom::augment(newdata = data, type.predict = 'response')
  }
  
  preds <-
    bind_rows(
      plays_w_pick_info %>% .f_predict() %>% mutate(grp_adj = 'Un-adjusted'),
      features_match %>% .f_predict() %>% mutate(grp_adj = 'Adjusted')
    ) %>% 
    mutate(
      across(c(grp), str_to_title)
    )
  preds

  viz_prop_probs <-
    preds %>% 
    ggplot() + 
    aes(x = .fitted, fill = grp) + 
    geom_histogram(data = . %>% filter(grp_adj == 'Un-adjusted'), fill = 'grey80', binwidth = 0.02, aes(y = -..count..)) +
    geom_histogram(data = . %>% filter(grp == 'Control', grp_adj == 'Adjusted'), binwidth = 0.02, aes(y = -..count..)) +
    geom_histogram(data = . %>% filter(grp == 'Treatment', grp_adj == 'Adjusted'), binwidth = 0.02) +
    scale_fill_manual(values = c(`Control` = 'dodgerblue', `Treatment` = 'darkorange')) +
    coord_cartesian(ylim = c(-ylim, ylim), clip = 'off') +
    guides(fill = guide_legend('')) +
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = 'top'
    ) +
    labs(
      title = sprintf('Probabiliites fit for %s model', title_suffix_1),
      caption = 'Gray illustrates un-adjusted samples, while colors illustrate adjusted samples.',
      y = '# of plays',
      x = sprintf('P(%s)', axis_label)
    )
  viz_prop_probs
  save_plot(viz_prop_probs, file = sprintf('viz_prop_probs_%s', file_suffix))
  
  sd_diffs <-
    bind_rows(
      plays_w_pick_info %>% 
        .aggregate_to_sd_diffs(col_trt = col_trt, fmla = fmla) %>% 
        mutate(grp_adj = 'Un-adjusted'),
      features_match %>% 
        .aggregate_to_sd_diffs(col_trt = col_trt, fmla = fmla) %>% 
        mutate(grp_adj = 'Adjusted')
    ) %>% 
    mutate(
      across(grp_adj, ~.x %>% fct_inorder())
    )
  sd_diffs
  
  sd_diffs_rnk <-
    sd_diffs %>% 
    filter(grp_adj == 'Un-adjusted') %>% 
    # arrange(desc(abs_diff)) %>% 
    mutate(rnk = row_number(desc(abs(sd_diff)))) %>% 
    select(col, rnk) %>% 
    arrange(rnk)
  sd_diffs_rnk

  viz_love <-
    sd_diffs %>% 
    left_join(sd_diffs_rnk) %>% 
    # .prep_viz_data() %>% 
    mutate(across(col, ~fct_reorder(.x, -rnk))) %>% 
    rename(lab = col) %>% 
    arrange(grp_adj, rnk) %>% 
    ggplot() +
    aes(y = lab, x = abs(sd_diff), color = grp_adj) +
    geom_vline(aes(xintercept = 0.1), size = 1, linetype = 2) +
    geom_point(size = 2) +
    geom_path(aes(group = grp_adj), size = 0.5) +
    scale_color_manual(values = c(`Un-adjusted` = 'dodgerblue', `Adjusted` = 'darkorange')) +
    guides(
      color = guide_legend('', override.aes = list(size = 3))
    ) +
    theme(
      axis.text.y = ggtext::element_markdown(),
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = 'top'
    ) +
    labs(
      title = sprintf('Bias among coefficients for %s probability model', title_suffix_1),
      x = 'Absolute standardized mean difference',
      y = NULL
    )
  viz_love
  save_plot(viz_love, file = sprintf('viz_love_%s', file_suffix))
  
  fmla_epa_chr <-
    fmla_chr %>% 
    # Unfortunately can't put `col_trt` on the left-hand side here, so replace it afterwards.
    str_replace_all(
      c(
        '~' = sprintf('~ %s + ', col_trt)
      )
    )
  fmla_epa_chr <- fmla_epa_chr %>% str_replace('.*[~]', 'epa ~')
  fmla_epa <- fmla_epa_chr %>% as.formula()
  
  fit_epa <-
    features_match %>% 
    lm(fmla_epa, data = .)
  fit_epa
  
  coefs_epa <-
    fit_epa %>%
    broom::tidy() %>%
    mutate(
      across(term, ~fct_reorder(.x, estimate)),
      lo = estimate - 1.96 * std.error,
      hi = estimate + 1.96 * std.error,
      is_signif = if_else(sign(lo) == sign(hi), TRUE, FALSE)
    )
  coefs_epa
  
  viz_epa <-
    coefs_epa %>%
    mutate(
      color = 
        case_when(
          str_detect(term, !!col_trt) ~ '#000000', #  gplots::col2hex('cornflowerblue'), #  gplots::col2hex('#0000ff'), 
          TRUE ~ gplots::col2hex('grey30')
        ),
      tag = 
        case_when(
          str_detect(term, !!col_trt) ~ 'b',
          TRUE ~ 'span'
        ),
      lab = glue::glue('<{tag} style="color:{color}">{term}</{tag}>'),
      across(lab, ~fct_reorder(.x, estimate))
    ) %>% 
    ggplot() +
    aes(y = lab, x = estimate, color = is_signif) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
    scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
    geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
    guides(color = guide_legend('Is statistically significant?')) +
    theme(
      axis.text.y = ggtext::element_markdown(),
      plot.title = element_text(size = 16, face = 'bold'),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.position = 'top' # ,
      # legend.box = element_rec()
    ) +
    labs(
      title = sprintf('Linear regression EPA model coefficients, adjusting for %s', title_suffix_2),
      y = NULL, x = 'Estimate +- 1.96 standard error'
    )
  viz_epa
  save_plot(viz_epa, file = sprintf('viz_epa_%s', file_suffix))
  features_match
}


# .get_valid_col_trt() %>% walk(do_causal_analysis)
features_match_is_target_picked <- 'is_target_picked' %>% do_causal_analysis()
features_match_has_same_defender <- 'has_same_defender' %>% do_causal_analysis()

# t test stuff ----
.simplify_pick_features <- function(data) {
  data %>% 
    mutate(
      across(c(is_target_picked, is_pass_successful, has_same_defender), binary_fct_to_lgl),
      across(
        is_target_picked, ~sprintf('Target Picked? %s', ifelse(.x, 'Y', 'N'))
      ),
      across(
        is_pass_successful, ~sprintf('Pass Successful? %s', ifelse(.x, 'Y', 'N'))
      ),
      across(
        has_same_defender, ~sprintf('Same Defender? %s', ifelse(.x, 'Y', 'N'))
      )
    ) %>%
    select(is_target_picked, is_pass_successful, has_same_defender, epa)
}

.get_valid_col_value <- function() {
  c('epa', 'n')
}

.validate_col_value <- function(x = .get_valid_col_value(), ...) {
  match.arg(x, ...)
}

.f_gtsummary <- function(data, by, col_value) {
  .validate_col_value(col_value)
  
  res <-
    data %>% 
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_integer() ~ '{N}',
        gtsummary::all_continuous() ~ '{mean} ({p25}, {p75})'
      ),
      by = all_of(by)
    )
  
  if(col_value == 'n') {
    return(res)
  }
  res %>% 
    gtsummary::add_p(
      test = gtsummary::all_continuous() ~ 't.test',
      # test = gtsummary::all_continuous() ~ '.t_test_epa',
      pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
    )
}

.f_col_gtsummary <- function(data, col, ..., col_value) {
  .validate_col_value(col_value)
  data %>% 
    select(!!sym(col), ...) %>% 
    .f_gtsummary(col, col_value = col_value)
}

.postprocess_t_test_tabs <- function(..., col_value, col, subtitle = NULL, suffix = NULL, sep = '_') {
  .validate_col_trt(col)
  .validate_col_value(col_value)
  # browser()
  res <-
    gtsummary::tbl_stack(
      list(...)
    )  %>% 
    gtsummary::as_gt() %>% 
    gt::tab_header(
      subtitle = subtitle,
      title = gt::md('EPA breakdown')
    )
  res
  
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }

  gt::gtsave(res, filename = file.path(get_bdb_dir_figs(), sprintf('tab_t_test_%s_%s%s.png', col_value, col, suffix)))
  res
}

.save_t_test_tabs <- function(data, col = 'is_target_picked', col_value = c('epa', 'n'), subtitle, suffix, ...) {
  
  .validate_col_trt(col)
  .validate_col_value(col_value)
  
  if(col == 'is_target_picked') {
    col_other <- 'has_same_defender'
    col_other_pretty <- 'Same Defender'
  } else {
    col_other <- 'is_target_picked'
    col_other_pretty <- 'Target Picked'
  }
  col_other_sym <- col_other %>% sym()
  col_value_pretty <- col_value %>% toupper()

  if(col_value == 'epa') {
    data['.value'] <- data$epa
  } else {
    data$.value <- 1L
  }

  datay <- 
    data %>%
    filter(!!col_other_sym %>% str_detect('Y$')) 
  
  datan <-
    data %>%
    filter(!!col_other_sym %>% str_detect('N$'))
  
  dataxy <-
    data %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  dataxn <-
    data %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  datayy <-
    datay %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  datayn <-
    datay %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  datany <-
    datan %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  datann <-
    datan %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  t <- data %>% .f_col_gtsummary(col, !!sym(sprintf('%s', col_value_pretty)) := .value, col_value = col_value)
  ty <- datay %>% .f_col_gtsummary(col, !!sym(sprintf('%s | %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  tn <- datan %>% .f_col_gtsummary(col, !!sym(sprintf('%s | %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  txy <- dataxy %>% .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y', col_value_pretty)) := .value, col_value = col_value)
  txn <- dataxn %>% .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N', col_value_pretty)) := .value, col_value = col_value)
  
  tyy <-
    datayy %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y & %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tyn <-
    datayn %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N & %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tny <-
    datany %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y & %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tnn <-
    datann %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N & %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  .postprocess_t_test_tabs(t, ty, tn, txy, txn, tyn, tny, tyy, tnn, ..., col_value = col_value, col = col, subtitle = subtitle, suffix = suffix)
}

do_save_t_test_tabs <- function(plays_w_pick_info, col_trt = .get_valid_col_trt(), subtitle, suffix) {
  .validate_col_trt(cols_trt, multiple.ok = TRUE)
  
  features_simple <- 
    plays_w_pick_info %>% 
    .simplify_pick_features()
  features_simple
  

  cols_value <- .get_valid_col_value()
  
  crossing(
    col = cols_trt,
    col_value = cols_value
  ) %>% 
    mutate(
      res = 
        map2(
          col, col_value, 
          ~.save_t_test_tabs(
            features_simple, 
            col = ..1, 
            col_value = ..2, 
            subtitle = subtitle, 
            suffix = suffix
          )
        )
    )
  features_simple
}

features_simple <- 
  plays_w_pick_info %>% 
  do_save_t_test_tabs(
    subtitle = 'Before matching',
    suffix = 'unadjusted'
  )

features_match_is_target_picked_simple <- 
  features_match_is_target_picked %>% 
  do_save_t_test_tabs(
    col_trt = 'is_target_picked',
    subtitle = 'After matching for targeted pick plays',
    suffix = 'adjusted_is_target_picked'
  )

features_match_has_same_defender_simple <- 
  features_match_has_same_defender %>% 
  do_save_t_test_tabs(
    col_trt = 'has_same_defender',
    subtitle = 'After matching for targeted defender coverage', 
    suffix = 'adjusted_has_same_defender'
  )

# visualize defenders ----
pick_plays <- plays_w_pick_def_info %>% filter(has_intersect)
.agg_plays_by <- function(data, ...) {
  data %>% 
    group_by(has_intersect, is_lo, ...) %>% 
    summarize(
      n = n(),
      across(matches('^[ew]pa'), list(sum = sum, mean = mean), na.rm = TRUE)
    ) %>% 
    ungroup()
}

.agg_plays_w_pick_def_info_by <- partial(.agg_plays_by, data = plays_w_pick_def_info, ... =)
.agg_pick_plays_by <- partial(.agg_plays_by, data = pick_plays, ... =)

plays_w_pick_def_info_by_defender <-
  .agg_plays_w_pick_def_info_by(
    is_target,
    has_same_defender,
    nfl_id_d_robust,
    display_name_d_robust,
    nfl_id_d_robust_init,
    display_name_d_robust_init
  )
plays_w_pick_def_info_by_defender

plays_w_pick_def_info_by_defender <-
  plays_w_pick_def_info_by_defender %>%
  group_by(
    nfl_id = nfl_id_d_robust,
    display_name = display_name_d_robust,
    is_target,
    has_intersect,
    has_same_defender
  ) %>%
  summarize(
    n = sum(n),
    across(matches('^[ew]pa.*sum$'), sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(nfl_id, display_name) %>%
  mutate(
    total = sum(n)
  ) %>%
  ungroup() %>%
  mutate(frac = n / total) %>% 
  arrange(desc(n))
plays_w_pick_def_info_by_defender

# Have to re-adjust total numbers to just the filtering criteria.
all_defenders_intersect_adj <-
  plays_w_pick_def_info_by_defender %>% 
  filter(is_target) %>% 
  group_by(nfl_id, display_name, is_target) %>% 
  mutate(
    total = sum(n)
  ) %>% 
  ungroup() %>%
  mutate(frac = n / total) %>% 
  filter(total >= 10) %>% 
  pivot_wider(
    names_from = c(has_intersect),
    values_from = c(n, frac, matches('^[ew]pa')),
    values_fill = 0
  )
all_defenders_intersect_adj

all_defenders_intersect_adj_agg <-
  all_defenders_intersect_adj %>% 
  group_by(nfl_id, display_name, is_target, total) %>% 
  summarize(across(where(is.double), sum)) %>% 
  ungroup()

defenders_intersect_adj_top_epa_t <-
  bind_rows(
    all_defenders_intersect_adj_agg %>% 
      arrange(epa_sum_TRUE) %>% 
      head(5) %>%
      mutate(top = TRUE),
    all_defenders_intersect_adj_agg %>% 
      arrange(-epa_sum_TRUE) %>% 
      head(5) %>% 
      mutate(top = FALSE)
  ) %>% 
  select(nfl_id, top)
defenders_intersect_adj_top_epa_t

defenders_intersect_adj_top_epa_f <-
  bind_rows(
    all_defenders_intersect_adj_agg %>% 
      anti_join(defenders_intersect_adj_top_epa_t) %>% 
      arrange(epa_sum_FALSE) %>% 
      head(5) %>%
      mutate(top = TRUE),
    all_defenders_intersect_adj_agg %>% 
      anti_join(defenders_intersect_adj_top_epa_t) %>% 
      arrange(-epa_sum_FALSE) %>% 
      head(5) %>% 
      mutate(top = FALSE)
  ) %>% 
  select(nfl_id, top)
defenders_intersect_adj_top_epa_f

defenders_intersect_adj_top_epa <-
  bind_rows(
    defenders_intersect_adj_top_epa_t %>% 
      mutate(pick = TRUE),
    defenders_intersect_adj_top_epa_f %>% 
      mutate(pick = FALSE)
  )
defenders_intersect_adj_top_epa

.common_defenders_intersect_adj_layers <- function(...) {
  list(
    ...,
    aes(y = epa_sum_FALSE, x = epa_sum_TRUE),
    geom_abline(
      data = tibble(intercept = seq(-30, 30, by = 10), slope = -1),
      aes(intercept = intercept, slope = slope),
      linetype = 2
    ),
    geom_point(
      aes(size = total), alpha = 0.2
    ),
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'dodgerblue'
    ),
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'dodgerblue'
    ),
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'darkorange'
    ),
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'darkorange'
    ),
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'indianred'
    ),
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'indianred'
    ),
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'forestgreen'
    ),
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'forestgreen'
    )
  )
}

viz_defenders_intersect_adj <-
  all_defenders_intersect_adj_agg %>% 
  ggplot() +
  .common_defenders_intersect_adj_layers() +
  geom_text(
    aes(x = 5.1, y = 45, label = 'Best on pick plays'), color = 'dodgerblue', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 42, label = 'Worst on pick plays'), color = 'darkorange', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 39, label = 'Best on non-pick plays'), color = 'indianred', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 36, label = 'Worst on non-pick plays'), color = 'forestgreen', size = pts(14), hjust = 0
  ) +
  guides(
    size = guide_legend(title = '# of total plays', override.aes = list(alpha = 1))
  ) +
  # coord_equal(xlim = c(-30, 30)) +
  theme(
    plot.caption = element_text(size = 10),
    legend.position = 'top'
  ) +
  labs(
    title = 'Aggreggate EPA when covering targeted receiver',
    caption = 'Minimum of 10 pick plays covered.\nPlot does not differentiate based on type of coverage (e.g. man). Defender at time of thow is used.',
    y = 'EPA on non-pick plays',
    x = 'EPA on pick plays'
  )
viz_defenders_intersect_adj
save_plot(viz_defenders_intersect_adj)

# Do the same thing, but splitting out by type of coverage (`has_same_defender`)
defenders_intersect_adj_by_coverage <-
  plays_w_pick_def_info_by_defender %>% 
  filter(is_target) %>% 
  group_by(nfl_id, display_name, is_target, has_same_defender) %>% 
  mutate(
    total = sum(n)
  ) %>% 
  ungroup() %>%
  mutate(frac = n / total) %>% 
  filter(total >= 10) %>% 
  pivot_wider(
    names_from = c(has_intersect),
    values_from = c(n, frac, matches('^[ew]pa')),
    values_fill = 0
  )
defenders_intersect_adj_by_coverage

viz_defenders_intersect_adj_by_coverage <-
  defenders_intersect_adj_by_coverage %>% 
  mutate(
    across(c(has_same_defender), binary_fct_to_lgl),
    across(
      has_same_defender, ~sprintf('Same Defender? %s', ifelse(.x, 'Y', 'N'))
    )
  ) %>% 
  ggplot() +
  .common_defenders_intersect_adj_layers() +
  guides(
    size = guide_legend(title = '# of total plays', override.aes = list(alpha = 1))
  ) +
  theme(
    plot.caption = element_text(size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = 'Aggreggate EPA when covering targeted receiver',
    subtitle = 'Differntiated by coverage type',
    caption = 'Minimum of 10 pick plays covered.',
    y = 'EPA on non-pick plays',
    x = 'EPA on pick plays'
  ) +
  facet_wrap(~has_same_defender)
viz_defenders_intersect_adj_by_coverage
save_plot(viz_defenders_intersect_adj_by_coverage)

# visualize receivers ----
pick_plays_by_receiver <- .agg_pick_plays_by(nfl_id, display_name)

.n_top <- 20L
viz_picks_by_receiver <-
  pick_plays_by_receiver %>%
  inner_join(
    pick_plays_by_receiver %>% 
      group_by(nfl_id, display_name) %>% 
      summarize(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(
        rnk = row_number(desc(total)),
      ) %>% 
      filter(rnk <= .n_top)
  ) %>% 
  mutate(
    across(display_name, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  # TODO: Replace these with above function.
  ggplot() +
  aes(x = n, y = display_name) +
  geom_col(aes(fill = is_lo)) +
  scale_fill_manual(values = c(`TRUE` = 'dodgerblue', `FALSE` = 'darkorange')) +
  guides(fill = guide_legend('Is Underneath Route Runner?')) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = '# of Pick Route Combinations Involved With',
    x = '# of Plays',
    y = NULL
  )
viz_picks_by_receiver
save_plot(viz_picks_by_receiver)

pick_plays_by_receiver_target <-
  .agg_pick_plays_by(
    nfl_id = nfl_id_target, 
    display_name = display_name_target
  ) %>% 
  filter(!is.na(display_name)) 
pick_plays_by_receiver_target

viz_picks_by_receiver_target <-
  pick_plays_by_receiver_target %>%
  inner_join(
    pick_plays_by_receiver_target %>% 
      group_by(nfl_id, display_name) %>% 
      summarize(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(
        rnk = row_number(desc(total)),
      ) %>% 
      filter(rnk <= .n_top)
  ) %>% 
  mutate(
    across(display_name, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  # TODO: Replace these with above function.
  ggplot() +
  aes(x = n, y = display_name) +
  geom_col() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = '# of targets When Involved In Pick Route Combination',
    x = '# of Plays',
    y = NULL
  )
viz_picks_by_receiver_target
save_plot(viz_picks_by_receiver_target)

plays_w_pick_def_info_by_receiver <- .agg_plays_w_pick_def_info_by(nfl_id, display_name)
pick_plays_by_receiver <-
  plays_w_pick_def_info_by_receiver %>% 
  group_by(nfl_id, display_name, has_intersect) %>% 
  summarize(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  group_by(nfl_id, display_name) %>% 
  mutate(
    total = sum(n),
    frac = n / total
  ) %>% 
  ungroup()
pick_plays_by_receiver

pick_plays_by_receiver_top <-
  pick_plays_by_receiver %>% 
  filter(has_intersect) %>% 
  filter(total > 200) %>% 
  arrange(desc(frac)) %>% 
  head(8)

viz_frac_by_receiver <-
  pick_plays_by_receiver %>% 
  # select(-total) %>% 
  pivot_wider(
    names_from = has_intersect,
    values_from = c(n, frac)
  ) %>% 
  ggplot() +
  aes(x = total, y = n_TRUE) +
  geom_abline(
    data = tibble(slope = seq(0.05, 0.25, by = 0.05)),
    aes(intercept = 0, slope = slope),
    linetype = 2
  ) +
  geom_point() +
  geom_point(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n),
    color = 'red'
  ) +
  ggrepel::geom_text_repel(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n, label = display_name),
    family = 'Karla',
    segment.color = 'red',
    segment.size = 0.2,
    color = 'red'
  ) +
  theme(
    plot.caption = element_text(size = 10),
  ) +
  labs(
    title = 'Relative Number of Pick Routes Ran',
    caption = 'Players with highest ratio of pick plays annotated (minimum 200 plays).\nLinear regression fit shown as dotted line',
    x = '# of other routes',
    y = '# of pick routes'
  )
viz_frac_by_receiver
save_plot(viz_frac_by_receiver)

# generic play examples ----
.filter_plays <- function(data) {
  data %>% 
    group_by(sec_intersect, before_cutoff, is_pass_successful, is_lo, is_target, has_same_defender) %>%
    mutate(prnk = percent_rank(epa)) %>%
    filter(prnk == min(prnk) | prnk == max(prnk)) %>%
    ungroup() %>%
    mutate(high_epa = if_else(prnk == 1, TRUE, FALSE)) %>%
    filter(high_epa == is_pass_successful) %>%
    arrange(sec_intersect, is_pass_successful, is_lo) %>%
    filter(is_lo)
}

pick_plays_meta_viz_init <-
  pick_plays %>% 
  # Basically like `is_pass_successful`.
  # mutate(pass_complete = if_else(pass_result == 'C', TRUE, FALSE)) %>%
  mutate(across(c(is_pass_successful, has_same_defender), binary_fct_to_lgl)) %>% 
  filter(is_target) %>%
  .filter_plays()
pick_plays_meta_viz_init

pick_plays_meta_viz <-
  pick_plays_meta_viz_init %>% 
  inner_join(plays %>% select(game_id, play_id, yards_gained = play_result)) %>% 
  mutate(
    lab = glue::glue('Pick between {display_name} ({jersey_number}) and {display_name_intersect} ({jersey_number_intersect}), {sec_intersect-0.5} <= t < {sec_intersect} seconds.
                     Target: {display_name_target} ({jersey_number_target}). Play result: {pass_result}.
                     BDB EPA: {scales::number(epa, accuracy = 0.01)}, nflfastR EPA: {scales::number(epa_nflfastr, accuracy = 0.01)}, nflfastR WPA: {scales::number(wpa_nflfastr, accuracy = 0.01)}'),
    path = file.path(
      bdb2021:::get_bdb_dir_figs(), 
      sprintf(
        'is_pick_play=%s-sec=%1.1f-is_pass_successful=%s-is_lo=%s-is_target=%s-high_epa=%s-%s-%s', 'Y', 
        sec_intersect, 
        ifelse(is_pass_successful, 'Y', 'N'), 
        ifelse(is_lo, 'Y', 'N'), 
        ifelse(is_target, 'Y', 'N'), 
        ifelse(high_epa, 'Y', 'N'), game_id, play_id)
    )
  )
pick_plays_meta_viz
pick_plays_meta_viz %>% filter(!before_cutoff)

primary_example_pick_plays <-
  list(
    pick_plays_meta_viz %>% 
      filter(before_cutoff) %>% 
      filter(is_target) %>% 
      filter(high_epa) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa'),
    pick_plays_meta_viz %>% 
      filter(before_cutoff) %>% 
      filter(is_target) %>% 
      filter(!high_epa) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa'),
    pick_plays_meta_viz %>% 
      filter(!before_cutoff) %>% 
      filter(is_target) %>% 
      filter(high_epa) %>% 
      filter(sec_intersect == 2.5) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_2.5s'),
    pick_plays_meta_viz %>% 
      filter(!before_cutoff) %>% 
      filter(is_target) %>% 
      filter(high_epa) %>% 
      filter(sec_intersect == 3) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_3.0s'),
    pick_plays_meta_viz %>% 
      filter(before_cutoff) %>% 
      filter(is_target) %>% 
      filter(high_epa) %>% 
      filter(sec_intersect == min(sec_intersect)) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'y_buffer')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play', descr))
  )
primary_example_pick_plays %>% select(descr, lab)

pick_plays_meta_viz_wo_primary <-
  pick_plays_meta_viz %>% 
  anti_join(primary_example_pick_plays %>% select(game_id, play_id))

secondary_example_pick_plays <-
  list(
    pick_plays_meta_viz_wo_primary %>% 
      filter(before_cutoff) %>% 
      filter(high_epa) %>% 
      filter(has_same_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_same_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(before_cutoff) %>% 
      filter(!high_epa) %>% 
      filter(has_same_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_same_defender')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play', descr))
  )
secondary_example_pick_plays %>% select(descr, lab)

pick_plays_meta_viz_wo_secondary <-
  pick_plays_meta_viz %>% 
  anti_join(secondary_example_pick_plays %>% select(game_id, play_id))

tertiary_example_pick_plays <-
  list(
    pick_plays_meta_viz_wo_secondary %>% 
      filter(before_cutoff) %>% 
      filter(high_epa) %>% 
      filter(!has_same_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_diff_defender'),
    pick_plays_meta_viz_wo_secondary %>% 
      filter(before_cutoff) %>% 
      filter(!high_epa) %>% 
      filter(!has_same_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_diff_defender')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play', descr))
  )
tertiary_example_pick_plays %>% select(descr, lab)

res_anim <-
  list(
    primary_example_pick_plays,
    secondary_example_pick_plays,
    tertiary_example_pick_plays
  ) %>% 
  reduce(bind_rows) %>% 
  select(descr, game_id, play_id, lab, path) %>% 
  # head(2) %>% 
  mutate(nearest_defender = if_else(str_detect(path, 'defender'), TRUE, FALSE)) %>% 
  mutate(
    across(path, ~.x %>% paste0('.gif')),
    anim = pmap(
      list(game_id, play_id, lab, path, nearest_defender),
      ~animate_play(
        game_id = ..1, 
        play_id = ..2,
        subtitle = ..3,
        plays = plays, 
        save = TRUE, 
        path = ..4, 
        nearest_defender = ..5, 
        target_probability = FALSE
      )
    )
  )

# xgboost stuff ----
# Reference: https://bradleyboehmke.github.io/HOML/iml.html#xgboost-and-built-in-shapley-values
.path_figs <- partial(file.path, get_bdb_dir_figs(), ... = )
do_save_shap_plots <- function(fit, mat, suffix = NULL, sep = '_', title = NULL, subtitle = NULL) {
  # mat <- model_mat_bdb
  # fit <- epa_model_bdb
  
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  
  feature_values_init <-
    mat %>%
    as.data.frame() %>%
    mutate_all(scale) %>%
    tidyr::gather('feature', 'feature_value') %>% 
    as_tibble()
  feature_values_init
  
  feature_values <-
    feature_values_init %>% 
    pull(feature_value)
  feature_values
  
  shap_init <-
    fit %>% 
    predict(newdata = mat, predcontrib = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>% 
    select(-matches('BIAS'))
  shap_init
  
  shap <- 
    shap_init %>% 
    mutate(idx = row_number()) %>% 
    pivot_longer(-idx, names_to = 'feature', values_to = 'shap_value')
  
  shap_agg_by_feature <-
    shap %>% 
    group_by(feature) %>% 
    summarize(
      across(shap_value, ~mean(abs(.x))),
    ) %>% 
    ungroup() %>% 
    mutate(
      across(shap_value, list(rnk = ~row_number(desc(.x))))
    ) %>% 
    arrange(shap_value_rnk)
  shap_agg_by_feature
  
  set.seed(42)
  shap_sample <- shap %>% group_by(feature) %>% sample_frac(0.1) %>% ungroup()
  
  .prep_viz_data <- function(data) {
    data %>% 
      mutate(
        color = 
          case_when(
            feature %in% .cols_trt ~ '#000000', 
            TRUE ~ gplots::col2hex('grey30')
          ),
        tag = 
          case_when(
            feature %in% .cols_trt ~ 'b',
            TRUE ~ 'span'
          ),
        lab = glue::glue('<{tag} style="color:{color}">{feature}</{tag}>'),
        across(lab, ~fct_reorder(.x, -shap_value_rnk))
      )
  }
  
  viz_shap_swarm <- 
    shap_sample %>% 
    left_join(shap_agg_by_feature %>% select(feature, shap_value_rnk)) %>% 
    .prep_viz_data() %>% 
    ggplot() +
    aes(x = shap_value, y = lab) +
    ggbeeswarm::geom_quasirandom(
      groupOnX = FALSE, 
      varwidth = TRUE, 
      color = '#4D4D4D',
      size = 0.2, 
      alpha = 0.3
    ) +
    theme(
      axis.text.y = ggtext::element_markdown()
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'SHAP value',
      y = NULL
    )
  
  save_plot(
    viz_shap_swarm,
    path = .path_figs(sprintf('shap_swarm%s.png', suffix))
  )
  
  viz_shap_agg <- 
    shap_agg_by_feature %>% 
    .prep_viz_data() %>% 
    ggplot() +
    aes(y = lab, x = shap_value) +
    geom_col() +
    theme(
      axis.text.y = ggtext::element_markdown(),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      y = NULL,
      x = 'mean(|SHAP value|)'
    )
  viz_shap_agg
  
  save_plot(
    viz_shap_agg,
    path = .path_figs(sprintf('shap_agg%s.png', suffix))
  )
  shap_agg_by_feature
}

epa_model_bdb <- xgboost::xgb.load(path_epa_model_bdb)
do_save_shap_plots(
  title = '{nflfastR} EPA model with added features',
  fit = epa_model_bdb,
  mat = model_mat_bdb
)

# .f_predict_bdb_epa <- function(object, newdata) {
#   newdata <- xgboost::xgb.DMatrix(data.matrix(newdata), missing = NA)
#   predict(epa_model_bdb, newdata)
# }
# 
# set.seed(42)
# # folds <- caret::createFolds(model_data_bdb$epa_bdb, k = 10, returnTrain = FALSE)
# # names(folds) <- NULL
# # idx <- folds[[1]]
# idx <- 1:nrow(model_mat_bdb)
# model_mat_bdb_sample <- model_mat_bdb[idx, ]
# df_bdb <- model_mat_bdb_sample %>% as.data.frame() %>% mutate(across(matches('[a-z][0-9]$'), factor))
# 
# pred <- 
#   iml::Predictor$new(
#     epa_model_bdb, 
#     data = df_bdb, 
#     y = model_data_bdb[idx, ][['epa_bdb']],
#     predict.fun = .f_predict_bdb_epa
#   )
# 
# fe_example <- 
#   iml::FeatureEffect$new(
#     pred, 
#     feature = c('yards_to_go'), 
#     method = 'ice'
#   )
# 
# viz_ice_example <- 
#   fe_example$plot(rug = FALSE, show.data = FALSE) + 
#   theme(
#     plot.title = element_text(size = 16)
#   ) +
#   labs(
#     title = 'ICE for yards to go'
#   )
# ggsave(plot = last_plot(), filename = .path_figs('ice_example.png'), width = 6, height = 6)
# 
# fe_is_target_picked <- 
#   iml::FeatureEffect$new(
#     pred, 
#     feature = c('roofdome'), 
#     method = 'ice'
#   )
# 
# viz_ice_is_target_picked <-
#   fe_is_target_picked$plot(rug = FALSE, show.data = FALSE) + 
#   theme(
#     # plot.title = element_text(size = 16),
#     panel.grid.major.x = element_blank()
#   ) +
#   labs(
#     title = 'ICE for tageted pick play'
#   )
# ggsave(plot = last_plot(), filename = .path_figs('ice_is_target_picked.png'), width = 6, height = 6)
# 
# fe_has_same_defender <- 
#   iml::FeatureEffect$new(
#     pred, 
#     feature = c('has_same_defender0'), 
#     method = 'ice'
#   )
# 
# viz_ice_has_same_defender <-
#   fe_has_same_defender$plot(rug = FALSE, show.data = FALSE) + 
#   theme(
#     # plot.title = element_text(size = 16),
#     panel.grid.major.x = element_blank()
#   ) +
#   labs(
#     title = 'ICE for targeted defender coverage'
#   )
# ggsave(plot = last_plot(), filename = .path_figs('ice_has_same_defender0.png'), width = 6, height = 6)
