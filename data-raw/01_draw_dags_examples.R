
# Reference: https://www.andrewheiss.com/blog/2020/02/25/closing-backdoors-dags/
library(tidyverse)  # ggplot, dplyr, %>%, and friends
library(ggdag)  # Make DAGs with ggplot
library(dagitty)  # Do basic DAG math
library(broom)  # For converting model output to data frames

node_details <- tribble(
  ~name, ~label, ~x, ~y,
  "math_camp", "Math camp", 2, 1,
  "final_grade", "Final grade", 4, 1,
  "needs_camp", "Needs camp", 1, 2,
  "gre_quant", "GRE quantitative", 2.5, 2,
  "gre_verbal", "GRE verbal", 5, 2,
  "background", "Background", 2, 3,
  "undergraduate_gpa", "Undergraduate GPA", 4, 3
)

node_labels <- node_details$label
names(node_labels) <- node_details$name
node_labels

math_camp_dag <- dagify(final_grade ~ math_camp + gre_quant + gre_verbal +
                          undergraduate_gpa + background,
                        math_camp ~ needs_camp,
                        needs_camp ~ background + undergraduate_gpa + gre_quant,
                        gre_quant ~ background + undergraduate_gpa,
                        gre_verbal ~ background + undergraduate_gpa,
                        undergraduate_gpa ~ background,
                        exposure = "math_camp",
                        outcome = "final_grade",
                        latent = "background",
                        coords = node_details,
                        labels = node_labels)

math_camp_dag

# Turn DAG into a tidy data frame for plotting
math_camp_dag_tidy <- math_camp_dag %>%
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent
math_camp_dag_tidy
status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

# Fancier graph
ggplot(math_camp_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) +
  theme_dag()

node_details_simpler <- tribble(
  ~name, ~label, ~x, ~y,
  "math_camp", "Math camp", 2, 1,
  "final_grade", "Final grade", 4, 1,
  "gre_quant", "GRE quantitative", 2.5, 2,
  "gre_verbal", "GRE verbal", 5, 2,
  "undergraduate_gpa", "Undergraduate GPA", 4, 3
)

node_labels_simpler <- node_details_simpler$label
names(node_labels_simpler) <- node_details_simpler$name

math_camp_dag_simpler <- dagify(final_grade ~ math_camp + gre_quant + gre_verbal +
                                  undergraduate_gpa,
                                math_camp ~ undergraduate_gpa + gre_quant,
                                gre_quant ~ undergraduate_gpa,
                                gre_verbal ~ undergraduate_gpa,
                                exposure = "math_camp",
                                outcome = "final_grade",
                                coords = node_details,
                                labels = node_labels)

# Turn DAG into a tidy data frame for plotting
math_camp_dag_simpler_tidy <- math_camp_dag_simpler %>%
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome
math_camp_dag_simpler_tidy

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

# Fancier graph
ggplot(math_camp_dag_simpler_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) +
  theme_dag()
