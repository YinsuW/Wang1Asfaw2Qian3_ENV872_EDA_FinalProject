## Table 1 Baseline characteristics


## Scatterplot of the relationship between PA and C-protein
PA.c.protein.plot<-ggplot(PA.c.protein,aes(x=PA,y=LBXCRP))+
  geom_point()
print(PA.c.protein.plot)


## Heatmap of PA and C-protein under different framingham score risks
ggplot(PA.c.protein, aes(risk_level,PA, z = LBXCRP)) +
  stat_summary_2d()  +
  viridis::scale_fill_viridis()

