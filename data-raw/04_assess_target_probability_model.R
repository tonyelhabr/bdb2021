
library(tidyverse)

paths_acc <-
  fs::dir_ls(
    'inst',
    regexp = 'acc.*csv$'
  )
paths_acc

paths_info_acc <-
  paths_acc %>%
  tibble(path = .) %>%
  mutate(
    file = path %>% basename() %>% tools::file_path_sans_ext()
  ) %>%
  separate(file, into = c('what', 'suffix', 'min_n', 'mtry', 'trees'), sep = '-') %>%
  mutate(
    across(
      c(min_n, mtry, trees),
      ~str_remove_all(.x, '[a-z_=]') %>% as.integer()
    )
  ) %>%
  select(-what)
paths_info_acc

grid_acc_init <-
  paths_info_acc %>%
  mutate(acc = map(path, read_csv)) %>%
  select(-path) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
grid_acc_init

acc <-
  grid_acc_init %>%
  unnest(acc)
acc

acc_filt <-
  acc %>%
  filter(.set == 'tst', .metric == 'accuracy', event != 'pass_forward')

grid_acc_filt_agg <-
  acc_filt %>%
  group_by(idx) %>%
  summarize(across(.estimate, mean)) %>%
  ungroup() %>%
  mutate(rnk = row_number(-.estimate)) %>%
  arrange(rnk) %>%
  left_join(grid_acc_init %>% select(-acc))
grid_acc_filt_agg

# acc %>%
#   filter(.set == 'tst', .metric == 'accuracy', event != 'pass_forward') %>%
#   summarize(across(.estimate, mean))

viz_acc_filt <-
  acc_filt %>%
  left_join(grid_acc_filt_agg %>% select(idx, rnk)) %>%
  filter(rnk <= 8) %>%
  # filter(event != 'pass_forward') %>%
  mutate(
    # across(idx, ordered),
    grp = sprintf('%d) suffix=%s, mtry=%d, min_n=%d', rnk, suffix, mtry, min_n) %>% fct_reorder(rnk),
    # sec = case_when(
    #   event == 'pass_forward' ~ 3.5,
    #   TRUE ~ event %>% str_remove(' sec') %>% as.double()
    # ),
    sec = event %>% str_remove(' sec') %>% as.double()
  ) %>%
  ggplot() +
  aes(x = sec, y = .estimate, color = grp) +
  geom_point(aes(size = -rnk), show.legend = FALSE) +
  geom_line() +
  guides(color = guide_legend(title = NULL, override.aes = list(size = 3))) +
  theme_classic() +
  theme(legend.position = 'right')
viz_acc_filt
