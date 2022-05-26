library(dz)

inst <- test_instances$p7_chao
variances = generate_variances(inst = inst)
info = generate_information(inst, r = 20)
p_inst = prepare_instance(inst, variances, info)

plot(p_inst)

ggsave("./figures_for_report/prepared_instance.png", width = 5, height = 5)
