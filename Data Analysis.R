## Question 1: Is physical activity associated with the level of C-protein? If so, what is the exact relationship? Is it dose-response? 

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.cap="C-protein Level under Different Physical Activity Status (Log-transformed)"}
PA.protein.lm.1<-lm(LBXCRP ~ PA,data = PA.c.protein)
PA.protein.lm.2<-lm(LBXCRP ~ PA+RIDAGEYR+RIAGENDR,data = PA.c.protein)
PA.protein.lm.3<-lm(LBXCRP ~ PA+RIDAGEYR+RIAGENDR+LBDHDDSI+LBDTCSI+SBP+SMQ040+DIQ010+RIDRETH1+DMDEDUC2+INDFMPIR+BMXBMI,data = PA.c.protein)
ggplot(PA.c.protein,aes(y=LBXCRP,x=PA))+
  geom_point()+
  geom_smooth(method  = "lm")+
  labs(x='Physical Activity (MET*Mins)',y="C-protein Level (mg/dL)")
```

## Question 2: Are different levels of framingham risk scores the effect modifiers for this relationship between physical activity and C-protein? How do they modify the relationship?


```{r echo=FALSE, message=FALSE, warning=FALSE,fig.cap="C-protein Level under Differen PA Status by Different Framingham Risk Scores (Log-tranformed)"}
PA.protein.interaction<-lm(LBXCRP ~ PA+PA*risk_level,data = PA.c.protein)
PA.protein.interaction<-lm(LBXCRP ~ PA++RIDAGEYR+RIAGENDR+PA*risk_level,data = PA.c.protein)
ggplot(PA.c.protein,aes(y=LBXCRP,x=PA,color=risk_level))+
  geom_point()+
  geom_smooth(method  = "lm")+
  labs(x='Physical Activity (MET*Mins)',y="C-protein Level (mg/dL)",color="Framingham Risk Score Levels")
```
