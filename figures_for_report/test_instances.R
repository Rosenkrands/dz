library(tidyverse)

inst_id <- sapply(1:20, function(i) paste0(7,".",letters[i]))
T_max <- seq(20, 400, length.out = 20)

df <- data.frame(T_max) |>
  mutate(`2` = as.integer(T_max/2), `3` = round(T_max/3, 2), `4` = as.integer(T_max/4)) |>
  select(-T_max)
rownames(df) <- inst_id

italic <- function(x){
  paste0('{\\emph{ ', x, '}}')
}

print(
  xtable::xtable(
    df,
    digits = 1
  ),
  sanitize.rownames.function = italic,
  sanitize.colnames.function = italic,
  booktabs = T
)

plot(dz::test_instances$p7_chao, delaunay = F)
ggsave("./figures_for_report/p7_chao.png", width = 5.5, height = 5)
