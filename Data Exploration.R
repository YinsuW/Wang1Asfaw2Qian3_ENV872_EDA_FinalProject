# Exploratory Analysis 

## Table 1 Baseline Characteristics of Participants
```{r echo=FALSE, message=FALSE, warning=FALSE}
PA.c.protein = PA.c.protein %>%mutate(gender = case_when(RIAGENDR==1 ~ 'Male', RIAGENDR==2 ~ 'Female'))
PA.c.protein = PA.c.protein %>%mutate(age = case_when(RIDAGEYR>=60 ~ 'Elder', RIDAGEYR<=59&RIDAGEYR>=40 ~ 'Middle Aged',RIDAGEYR<=39&RIDAGEYR>=20 ~ 'Young'))
PA.c.protein = PA.c.protein %>%mutate(familyPIR = case_when(INDFMPIR>=0&INDFMPIR<1 ~ '0≤PIR<1', INDFMPIR>=1&INDFMPIR<2 ~ '1≤PIR<2',INDFMPIR>=2&INDFMPIR<3~'2≤PIR<3',INDFMPIR>=3~'PIR ≥3'))
PA.c.protein = PA.c.protein %>%mutate(race = case_when(RIDRETH1==3 ~ 'Non-Hispanic White', RIDRETH1==4 ~ 'Non-Hispanic Black',(RIDRETH1==1) |(RIDRETH1==2)~'Hispanic',RIDRETH1==5~'Other'))
PA.c.protein = PA.c.protein %>%mutate(education = case_when(DMDEDUC2 <=2 ~ 'Below high school', DMDEDUC2==3 ~ 'High school',(DMDEDUC2==4) |(DMDEDUC2==5)~'College or above'))
label(PA.c.protein$age) <- "Age"
label(PA.c.protein$gender) <- "Gender"
label(PA.c.protein$race) <- "Ethnicity"
label(PA.c.protein$education) <- "Education"
label(PA.c.protein$familyPIR) <- "Family PIR"
label(PA.c.protein$SBP) <- "Systolic Blood Pressure (mmHg)"
label(PA.c.protein$LBDHDDSI) <- "Direct HDL-Cholesterol (mmol/L)"
label(PA.c.protein$LBDTCSI) <- "Total Cholesterol (mmol/L)"
label(PA.c.protein$BMXBMI) <- "Body Mass Index (kg/m2)"
table1(~age + gender+ race + education + familyPIR + SBP + LBDHDDSI + LBDTCSI + BMXBMI, data=PA.c.protein, overall="Total", caption = "Baseline Characteristics of Participants")
```
## Scatterplot of the relationship between PA and C-protein
```{r echo=FALSE, message=FALSE, warning=FALSE,fig.cap="The relationship between Physical Activity and C-protein Concentration"}
PA.c.protein.plot<-ggplot(PA.c.protein,aes(x=PA,y=LBXCRP))+
  geom_point()+
  labs(x="Physical Activity (MET*Mins)",y="C-protein Concentration (mg/dL)")
print(PA.c.protein.plot)
```

```{r echo=FALSE, message=FALSE, warning=FALSE,fig.cap="The relationship between Physical Activity and C-protein Concentration (Log-transformed)"}
PA.c.protein$PA<-log(PA.c.protein$PA)
PA.c.protein$LBXCRP<-log(PA.c.protein$LBXCRP)
PA.c.protein.plot<-ggplot(PA.c.protein,aes(x=PA,y=LBXCRP))+
  geom_point()+
  labs(x="Physical Activity (MET*Mins)",y="C-protein Concentration (mg/dL)")
print(PA.c.protein.plot)
```

## Heatmap of PA and C-protein under different Framingham risk scores
```{r echo=FALSE, message=FALSE, warning=FALSE,fig.cap="C-protein Levels under Physical Activity Status and Framingham Risk Scores (Log-transformed)"}
ggplot(PA.c.protein, aes(risk_level,PA, z = LBXCRP)) +
  stat_summary_2d()  +
  viridis::scale_fill_viridis()+
  labs(x="Framingham risk scores",y="Physical Activity (MET*Mins)")+ 
  guides(fill=guide_legend(title="C-protein (mg/dL)"))
```
