rna %<>% dplyr::filter(`Gene name` %in% c(anova.res_conserve$path,anova.res_specifc$path))

rna2 <- rna %>% pivot_wider(names_from = Tissue2,values_from = nTPM) %>% 
  group_by(`Gene name`) %>% 
  summarise(wuguan=mean(wuguan,na.rm = T),
            other=mean(other,na.rm = T),
            Brain=mean(Brain,na.rm = T),
            sex=mean(sex,na.rm = T))

p1<-rna2[rna2$`Gene name` %in% anova.res_specifc$path,] %>% 
  pivot_longer(cols = wuguan:sex,names_to = "Tissue2") %>% 
  #dplyr::filter(Tissue2 %in% Tissuelist[1:27]) %>% 
  ggplot(aes(x = Tissue2,y=value))+
  geom_boxplot()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 45,hjust= 1),
        axis.text = element_text(colour = "black"))+
  xlab("")+ylab("log2(TPM+1)")+
  ggtitle("Variant Genes")

p2<-rna2[rna2$`Gene name` %in% anova.res_conserve$path,] %>% 
  pivot_longer(cols = wuguan:sex,names_to = "Tissue2") %>% 
  #dplyr::filter(Tissue2 %in% Tissuelist[1:27]) %>% 
  ggplot(aes(x = Tissue2,y=value))+
  geom_boxplot()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 45,hjust= 1),
        axis.text = element_text(colour = "black"))+
  xlab("")+ylab("")+
  ggtitle("Conserve Genes")
p1+p2