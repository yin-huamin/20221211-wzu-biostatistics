library(patchwork)

Tissuelist <- rna %>% group_by(Tissue) %>% summarise(nTPM=mean(nTPM)) %>% arrange(desc(nTPM)) %>% dplyr::pull(Tissue)

p1 <- rna[sample(1:22000,5000),] %>% 
  dplyr::filter(Tissue %in% Tissuelist[1:27]) %>% 
  ggplot(aes(x = Tissue,y=log2(nTPM+1)))+
  geom_boxplot()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 45,hjust= 1),
        axis.text = element_text(colour = "black"))+
  xlab("")

p2 <- rna[sample(1:22000,5000),] %>% 
  dplyr::filter(Tissue %in% Tissuelist[28:54]) %>% 
  ggplot(aes(x = Tissue,y=log2(nTPM+1)))+
  geom_boxplot()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 45,hjust= 1),
        axis.text = element_text(colour = "black"))

p1/p2
ggsave("./tissue_tpm_boxplot.png",dpi = 300,height = 5,width = 10)