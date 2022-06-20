library(tidyverse)

inst <- test_instances$p7_chao
variances <- generate_variances(inst)
info <- generate_information(inst)
p_inst <- prepare_instance(inst, variances, info)

clust <- clustering(p_inst, k = 4, L = 120, eps = 0, variances, info, cluster_method = "local_search", alpha = 1)

plot(clust)

animate_local_search(clust, filename = "HC_v2.gif")

# ggsave("HC_v2.png", width = 1920, height = 1080, units = "px", dpi = 250)
