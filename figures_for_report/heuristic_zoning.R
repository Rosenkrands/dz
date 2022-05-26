library(dz)
library(tidyverse)
set.seed(35)

inst = test_instances$p7_chao
L = 100
k = 4
variances = generate_variances(inst = inst)
info = generate_information(inst, r = 20)
p_inst = prepare_instance(inst, variances, info)

clust_gr <- clustering(inst = p_inst, k, L, eps = 0, variances, info, cluster_method = "greedy")
plot(clust_gr)
ggsave("./figures_for_report/greedy.png", width = 5, height = 5)

clust_ls <- clustering(inst = p_inst, k, L, eps = 0, variances = NULL, info = NULL, cluster_method = "local_search")
plot(clust_ls)
ggsave("./figures_for_report/local_search.png", width = 5, height = 5)
