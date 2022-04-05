
nhanes1<-subset(nhanes,select = c("SEQN",'RIDAGEYR','LBDHDDSI','LBDTCSI','BPQ040A','SMQ040','DIQ010','PAQ605','PAQ610','PAD615','PAQ620','PAQ625','PAD630','PAQ635','PAQ640','PAD645','RIDRETH1','DMDEDUC2','INDFMPIR','BMXBMI','LBXCRP','ucod_leading',"BPXSY1","BPXSY2","BPXSY3","RIAGENDR"))
nhanes1<-setDT(nhanes1)[, lapply(.SD, na.omit), by = SEQN]
nhanes1<-unique(nhanes1)

nhanes1<-nhanes1 %>% replace_with_na(replace = list(PAQ605 = 9))%>% 
  replace_with_na(replace = list(PAQ610 = 99))%>% 
  replace_with_na(replace = list(PAD615 = 9999))%>% 
  replace_with_na(replace = list(PAQ620 = 9))%>% 
  replace_with_na(replace = list(PAQ625 = 99))%>% 
  replace_with_na(replace = list(PAD630 = 9999))%>% 
  replace_with_na(replace = list(PAQ635 = 9))%>% 
  replace_with_na(replace = list(PAQ640 = 99))%>% 
  replace_with_na(replace = list(PAD645 = 9999))
nhanes1<-nhanes1 %>% replace_with_na(replace = list(PAQ605 = 7))%>% 
  replace_with_na(replace = list(PAQ610 = 77))%>% 
  replace_with_na(replace = list(PAD615 = 7777))%>% 
  replace_with_na(replace = list(PAQ620 = 7))%>% 
  replace_with_na(replace = list(PAQ625 = 77))%>% 
  replace_with_na(replace = list(PAD630 = 7777))%>% 
  replace_with_na(replace = list(PAQ635 = 7))%>% 
  replace_with_na(replace = list(PAQ640 = 77))%>% 
  replace_with_na(replace = list(PAD645 = 7777))
nhanes1<-nhanes1[!is.na(PAQ605)|!is.na(PAQ620)|is.na(PAQ635)]
nhanes1<-nhanes1%>%mutate(vigorous = (nhanes1$PAD615)*(nhanes1$PAQ610)*8)%>%
  mutate(moderate_walk = (nhanes1$PAD645)*(nhanes1$PAQ640)*4)%>%
  mutate(moderate_activity = (nhanes1$PAQ625)*(nhanes1$PAD630)*4)
nhanes1<-subset(nhanes1,is.na(moderate_activity)==FALSE | is.na(moderate_walk)==FALSE | is.na(vigorous)==FALSE)

nhanes1 <- nhanes1 %>% mutate(SBP = 
                                case_when(BPXSY1>0&is.na(BPXSY2)&is.na(BPXSY3) ~ BPXSY1,
                                          BPXSY1>0&BPXSY2>0&is.na(BPXSY3) ~ BPXSY2,
                                          BPXSY1>0&BPXSY2>0&BPXSY3>0 ~ (BPXSY2+BPXSY3)/2))

nhanes2<-nhanes1[complete.cases(nhanes1[,c('RIDAGEYR','LBDHDDSI','LBDTCSI','SMQ040','DIQ010','SBP')]),]
nhanes2=filter(filter(filter(filter(filter(filter(nhanes2,SMQ040 != 7),SMQ040 != 9),DIQ010 != 7),DIQ010 != 9),BPQ040A !=7),BPQ040A!=9)
nhanes2=filter(nhanes2,RIDAGEYR>=20)

nhanes21<-subset(nhanes2,select = c("SEQN",'RIDAGEYR','LBDHDDSI','LBDTCSI','BPQ040A','SMQ040','DIQ010','PAQ605','PAQ610','PAD615','PAQ620','PAQ625','PAD630','PAQ635','PAQ640','PAD645','RIDRETH1','DMDEDUC2','INDFMPIR','BMXBMI','LBXCRP','ucod_leading',"BPXSY1","BPXSY2","BPXSY3","RIAGENDR","vigorous","SBP"))
nhanes21<-rename(nhanes21, PA = vigorous)

nhanes22<-subset(nhanes2,select = c("SEQN",'RIDAGEYR','LBDHDDSI','LBDTCSI','BPQ040A','SMQ040','DIQ010','PAQ605','PAQ610','PAD615','PAQ620','PAQ625','PAD630','PAQ635','PAQ640','PAD645','RIDRETH1','DMDEDUC2','INDFMPIR','BMXBMI','LBXCRP','ucod_leading',"BPXSY1","BPXSY2","BPXSY3","RIAGENDR","moderate_activity","SBP"))
nhanes22<-rename(nhanes22, PA = moderate_activity)
nhanes23<-subset(nhanes2,select = c("SEQN",'RIDAGEYR','LBDHDDSI','LBDTCSI','BPQ040A','SMQ040','DIQ010','PAQ605','PAQ610','PAD615','PAQ620','PAQ625','PAD630','PAQ635','PAQ640','PAD645','RIDRETH1','DMDEDUC2','INDFMPIR','BMXBMI','LBXCRP','ucod_leading',"BPXSY1","BPXSY2","BPXSY3","RIAGENDR","moderate_walk","SBP"))
nhanes23<-rename(nhanes23, PA = moderate_walk)
nhanes3<-rbind(nhanes21,nhanes22,nhanes23)
nhanes3<-nhanes3[!is.na(nhanes3$PA)]

# framingham score
if (nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(score_Age = 
                                                     case_when  
                                                   ((nhanes3$RIDAGEYR >=30) & (nhanes3$RIDAGEYR<=34) ~ 0,
                                                     (nhanes3$RIDAGEYR >=35) & (nhanes3$RIDAGEYR <=39) ~2,
                                                     (RIDAGEYR >=40) & (RIDAGEYR <=44) ~5,
                                                     (RIDAGEYR >=45) & (RIDAGEYR <=49) ~7,
                                                     (RIDAGEYR >=50) & (RIDAGEYR <=54) ~8,
                                                     (RIDAGEYR >=55) & (RIDAGEYR <=59) ~10,
                                                     (RIDAGEYR >=60) & (RIDAGEYR <=64) ~11,
                                                     (RIDAGEYR >=65) & (RIDAGEYR <=69) ~12,
                                                     (RIDAGEYR >=70) & (RIDAGEYR <=74) ~14,
                                                     (RIDAGEYR >=75) ~15))}else{nhanes3%>%mutate(score_Age = 
                                                                                                   case_when  
                                                                                                 ((RIDAGEYR >=30) & (RIDAGEYR <=34) ~ 0,
                                                                                                   (RIDAGEYR >=35) & (RIDAGEYR <=39) ~2,
                                                                                                   (RIDAGEYR >=40) & (RIDAGEYR <=44) ~4,
                                                                                                   (RIDAGEYR >=45) & (RIDAGEYR <=49) ~5,
                                                                                                   (RIDAGEYR >=50) & (RIDAGEYR <=54) ~7,
                                                                                                   (RIDAGEYR >=55) & (RIDAGEYR <=59) ~8,
                                                                                                   (RIDAGEYR >=60) & (RIDAGEYR <=64) ~9,
                                                                                                   (RIDAGEYR >=65) & (RIDAGEYR <=69) ~10,
                                                                                                   (RIDAGEYR >=70) & (RIDAGEYR <=74) ~11,
                                                                                                   (RIDAGEYR >=75) ~12))}

if (nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(score_HDL = 
                                                     case_when((nhanes3$LBDHDDSI>1.6) ~ -2,
                                                               (nhanes3$LBDHDDSI>=1.3) & (nhanes3$LBDHDDSI <=1.6) ~ -1,
                                                               (nhanes3$LBDHDDSI>=1.2) & (nhanes3$LBDHDDSI <=1.29) ~ 0,
                                                               (nhanes3$LBDHDDSI>=0.9) & (nhanes3$LBDHDDSI <=1.19) ~ 1,
                                                               (nhanes3$LBDHDDSI<0.9) ~ 2))}else{nhanes3<-nhanes3%>%mutate(score_HDL = 
                                                                                                                             case_when((nhanes3$LBDHDDSI>1.6) ~ -2,
                                                                                                                                       (nhanes3$LBDHDDSI>=1.3) & (nhanes3$LBDHDDSI <=1.6) ~ -1,
                                                                                                                                       (nhanes3$LBDHDDSI>=1.2) & (nhanes3$LBDHDDSI <=1.29) ~ 0,
                                                                                                                                       (nhanes3$LBDHDDSI>=0.9) & (nhanes3$LBDHDDSI <=1.19) ~ 1,
                                                                                                                                       (nhanes3$LBDHDDSI<0.9) ~ 2))}
if (nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(score_TC = 
                                                     case_when((nhanes3$LBDTCSI>7.2) ~ 4,
                                                               (nhanes3$LBDTCSI>=6.2) & (nhanes3$LBDTCSI <=7.2) ~ 3,
                                                               (nhanes3$LBDTCSI>=5.2) & (nhanes3$LBDTCSI <=6.19) ~ 2,
                                                               (nhanes3$LBDTCSI>=4.1) & (nhanes3$LBDTCSI <=5.19) ~ 1,
                                                               (nhanes3$LBDTCSI<4.1) ~ 0))}else{nhanes3<-nhanes3%>%mutate(score_TC = 
                                                                                                                            case_when((nhanes3$LBDTCSI>7.2) ~ 5,
                                                                                                                                      (nhanes3$LBDTCSI>=6.2) & (nhanes3$LBDTCSI <=7.2) ~ 4,
                                                                                                                                      (nhanes3$LBDTCSI>=5.2) & (nhanes3$LBDTCSI <=6.19) ~ 3,
                                                                                                                                      (nhanes3$LBDTCSI>=4.1) & (nhanes3$LBDTCSI <=5.19) ~ 1,
                                                                                                                                      (nhanes3$LBDTCSI<4.1) ~ 0))}

if (nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(score_BP = 
                                                     if(nhanes3$BPQ040A==1) {case_when((nhanes3$SBP>160) ~ 5,
                                                                                       (nhanes3$SBP>=150) & (nhanes3$SBP <=159) ~ 4,
                                                                                       (nhanes3$SBP>=140) & (nhanes3$SBP <=149) ~ 4,
                                                                                       (nhanes3$SBP>=130) & (nhanes3$SBP <=139) ~ 3,
                                                                                       (nhanes3$SBP>=120) & (nhanes3$SBP <=129) ~ 2,
                                                                                       (nhanes3$SBP<120) ~ 0)}else
                                                                                         if(nhanes3$BPQ040A==2) {case_when((nhanes3$SBP>160) ~ 3,
                                                                                                                           (nhanes3$SBP>=150) & (nhanes3$SBP <=159) ~ 2,
                                                                                                                           (nhanes3$SBP>=140) & (nhanes3$SBP <=149) ~ 2,
                                                                                                                           (nhanes3$SBP>=130) & (nhanes3$SBP <=139) ~ 1,
                                                                                                                           (nhanes3$SBP>=120) & (nhanes3$SBP <=129) ~ 0,
                                                                                                                           (nhanes3$SBP<120) ~ -2)})}else{nhanes3<-nhanes3%>%mutate(score_BP = 
                                                                                                                                                                                      if(nhanes3$BPQ040A==1) {case_when((nhanes3$SBP>160) ~ 7,
                                                                                                                                                                                                                        (nhanes3$SBP>=150) & (nhanes3$SBP <=159) ~ 6,
                                                                                                                                                                                                                        (nhanes3$SBP>=140) & (nhanes3$SBP <=149) ~ 5,
                                                                                                                                                                                                                        (nhanes3$SBP>=130) & (nhanes3$SBP <=139) ~ 3,
                                                                                                                                                                                                                        (nhanes3$SBP>=120) & (nhanes3$SBP <=129) ~ 2,
                                                                                                                                                                                                                        (nhanes3$SBP<120) ~ -1)}else
                                                                                                                                                                                                                          if(nhanes3$BPQ040A==2) {case_when((nhanes3$SBP>160) ~ 5,
                                                                                                                                                                                                                                                            (nhanes3$SBP>=150) & (nhanes3$SBP <=159) ~ 4,
                                                                                                                                                                                                                                                            (nhanes3$SBP>=140) & (nhanes3$SBP <=149) ~ 2,
                                                                                                                                                                                                                                                            (nhanes3$SBP>=130) & (nhanes3$SBP <=139) ~ 1,
                                                                                                                                                                                                                                                            (nhanes3$SBP>=120) & (nhanes3$SBP <=129) ~ 0,
                                                                                                                                                                                                                                                            (nhanes3$SBP<120) ~ -3)})}

if(nhanes3$RIAGENDR==2){nhanes3<-nhanes3%>%mutate(score_Smoke =
                                                    case_when((nhanes3$SMQ040==1) ~ 3,
                                                              (nhanes3$SMQ040==2) ~ 3,
                                                              (nhanes3$SMQ040==3) ~ 0))}else
                                                              {nhanes3<-nhanes3%>%mutate(score_Smoke =
                                                                                           case_when((nhanes3$SMQ040==1) ~ 4,
                                                                                                     (nhanes3$SMQ040==2) ~ 4,
                                                                                                     (nhanes3$SMQ040==3) ~ 0))}
if(nhanes3$RIAGENDR==2){nhanes3<-nhanes3%>%mutate(score_Diabetes =
                                                    case_when((nhanes3$SMQ040==1) ~ 4,
                                                              (nhanes3$SMQ040==2) ~ 4,
                                                              (nhanes3$SMQ040==3) ~ 0))}else
                                                              {nhanes3<-nhanes3%>%mutate(score_Diabetes =
                                                                                           case_when((nhanes3$SMQ040==1) ~ 3,
                                                                                                     (nhanes3$SMQ040==2) ~ 3,
                                                                                                     (nhanes3$SMQ040==3) ~ 0))}
nhanes3<-nhanes3%>%mutate(score=score_Age+score_BP+score_HDL+score_TC+score_Smoke+score_Diabetes)

if(nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(risk =
                                                    case_when((nhanes3$score <= -3) ~ 1,
                                                              (nhanes3$score==-2) ~ 1.1,
                                                              (nhanes3$score==-1) ~ 1.4,
                                                              (nhanes3$score==0) ~ 1.6,
                                                              (nhanes3$score==1) ~ 1.9,
                                                              (nhanes3$score==2) ~ 2.3,
                                                              (nhanes3$score==3) ~ 2.8,
                                                              (nhanes3$score==4) ~ 3.3,
                                                              (nhanes3$score==5) ~ 3.9,
                                                              (nhanes3$score==6) ~ 4.7,
                                                              (nhanes3$score==7) ~ 5.6,
                                                              (nhanes3$score==8) ~ 6.7,
                                                              (nhanes3$score==9) ~ 7.9,
                                                              (nhanes3$score==10) ~ 9.4,
                                                              (nhanes3$score==11) ~ 11.2,
                                                              (nhanes3$score==12) ~ 13.3,
                                                              (nhanes3$score==13) ~ 15.6,
                                                              (nhanes3$score==14) ~ 18.4,
                                                              (nhanes3$score==15) ~ 21.6,
                                                              (nhanes3$score==16) ~ 25.3,
                                                              (nhanes3$score==17) ~ 29.4,
                                                              (nhanes3$score==18) ~ 30,
                                                              (nhanes3$score==19) ~ 30,
                                                              (nhanes3$score==20) ~ 30,
                                                              (nhanes3$score>=21) ~ 30,))}else
                                                              {nhanes3<-nhanes3%>%mutate(risk =
                                                                                           case_when((nhanes3$score <= -3) ~ 1,
                                                                                                     (nhanes3$score==-2) ~ 1,
                                                                                                     (nhanes3$score==-1) ~ 1,
                                                                                                     (nhanes3$score==0) ~ 1.2,
                                                                                                     (nhanes3$score==1) ~ 1.5,
                                                                                                     (nhanes3$score==2) ~ 1.7,
                                                                                                     (nhanes3$score==3) ~ 2.0,
                                                                                                     (nhanes3$score==4) ~ 2.4,
                                                                                                     (nhanes3$score==5) ~ 2.8,
                                                                                                     (nhanes3$score==6) ~ 3.3,
                                                                                                     (nhanes3$score==7) ~ 3.9,
                                                                                                     (nhanes3$score==8) ~ 4.5,
                                                                                                     (nhanes3$score==9) ~ 5.3,
                                                                                                     (nhanes3$score==10) ~ 6.3,
                                                                                                     (nhanes3$score==11) ~ 7.3,
                                                                                                     (nhanes3$score==12) ~ 8.6,
                                                                                                     (nhanes3$score==13) ~ 10.0,
                                                                                                     (nhanes3$score==14) ~ 11.7,
                                                                                                     (nhanes3$score==15) ~ 13.7,
                                                                                                     (nhanes3$score==16) ~ 15.9,
                                                                                                     (nhanes3$score==17) ~ 18.51,
                                                                                                     (nhanes3$score==18) ~ 21.5,
                                                                                                     (nhanes3$score==19) ~ 24.8,
                                                                                                     (nhanes3$score==20) ~ 27.5,
                                                                                                     (nhanes3$score>=21) ~ 30,))}

#risk_level definition
if (nhanes3$RIAGENDR==1){nhanes3<-nhanes3%>%mutate(risk_level = 
                                                     case_when  
                                                   ((risk <=10) ~ "low",
                                                     (risk >10) & (risk <=14) ~"intermediate",
                                                     (risk >14) ~"high"))}else{nhanes3%>%mutate(risk_level = 
                                                                                                  case_when  
                                                                                                ((risk <=12) ~ "low",
                                                                                                  (risk >12) & (risk <=17) ~"intermediate",
                                                                                                  (risk >17) ~"high"))}

#PA_level definition
nhanes3<-nhanes3%>%mutate(PA_level = 
                            case_when  
                          ((PA <=150) ~ "low",
                            (PA >150) & (PA <=1118) ~"intermediate",
                            (PA >1118) ~"high"))

nhanes3<-nhanes3[!is.na(nhanes3$score)]
hanes3<-nhanes3[!is.na(nhanes3$risk)]
PA.c.protein<-nhanes3[!is.na(nhanes3$risk_level)]