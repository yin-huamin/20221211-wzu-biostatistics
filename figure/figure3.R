rna$nTPM<-log2(rna$nTPM+1)
rna_delete <- rna %>% 
  group_by(`Gene name`) %>% 
  summarise(variance = var(nTPM),
            means = mean(nTPM)) %>% 
  dplyr::filter(variance==0 & means==0)

rna_var <- rna %>% group_by(`Gene name`) %>% summarise(variance = var(nTPM)) %>% dplyr::arrange(desc(variance))

rna_var_glist <- c(head(rna_var$`Gene name`),tail(rna_var$`Gene name`))

rna %>% 
  dplyr::filter(`Gene name` %in% rna_var_glist) %>% 
  ggplot(aes(x = `Gene name`,y=nTPM))+
    geom_boxplot()+
    theme_test()+
    theme(axis.text.x = element_text(angle = 45,hjust= 1),
          axis.text = element_text(colour = "black"))+
    xlab("")

ggsave("./tissue_var-glist_boxplot.png",dpi = 300,height = 3,width = 10)