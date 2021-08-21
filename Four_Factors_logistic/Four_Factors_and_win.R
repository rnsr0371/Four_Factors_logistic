#必要なライブラリのインポート
library(tidyverse)

#データのインポート
teams=read_csv("../data/teams.csv")
score=read_csv("../data/games_summary_202021.csv")

#teamsから2020-21シーズンのB1のデータだけ抽出
teams=teams %>% filter(Season=="2020-21") %>% filter(League=="B1")

#scoreからB1のデータだけ抽出
score=teams %>% left_join(score,by="TeamId")

#scoreに勝敗を追加する
score=score %>% arrange(ScheduleKey)
k=seq(1,1147,2)
g=seq(2,1148,2)

diff_score_k=score$PTS[k]-score$PTS[g]
diff_score_g=score$PTS[g]-score$PTS[k]

score=rbind(score[k,],score[g,])
score=score %>% mutate(diff=c(diff_score_k,diff_score_g))

score=score %>% mutate(win=ifelse(diff>0,1,0))#勝った場合は1

score=score %>% arrange(ScheduleKey)

#FourFactorsを計算する
#eFG
eFG_k=(score$F2GM[g]+1.5*score$F3GM[g])/(score$F2GA[g]+score$F3GA[g])
eFG_g=(score$F2GM[k]+1.5*score$F3GM[k])/(score$F2GA[k]+score$F3GA[k])
score=rbind(score[k,],score[g,])
score=score %>% mutate(D_eFG=c(eFG_k,eFG_g))
score=score %>% mutate(eFG=(F2GM+1.5*F3GM)/(F2GA+F3GA))
score=score %>% arrange(ScheduleKey)

#TO%
TOP_k=(score$TO[g]/(score$F2GA[g]+score$F3GA[g]+0.44*score$FTA[g]+score$TO[g]))
TOP_g=(score$TO[k]/(score$F2GA[k]+score$F3GA[k]+0.44*score$FTA[k]+score$TO[k]))
score=rbind(score[k,],score[g,])
score=score %>% mutate(D_TOP=c(TOP_k,TOP_g))
score=score %>% mutate(TOP=TO/(F2GA+F3GA+0.44*FTA+TO))
score=score %>% arrange(ScheduleKey)

#FT%
FTP_k=(score$FTA[g]/(score$F2GA[g]+score$F3GA[g]))
FTP_g=(score$FTA[k]/(score$F2GA[k]+score$F3GA[k]))
score=rbind(score[k,],score[g,])
score=score %>% mutate(D_FTP=c(FTP_k,FTP_g))
score=score %>% mutate(FTR=FTA/(F2GA+F3GA))
score=score %>% arrange(ScheduleKey)

#ORBP
ORBP_g=score$OR[k]/(score$OR[k]+score$DR[g])
ORBP_k=score$OR[g]/(score$OR[g]+score$DR[k])
score=rbind(score[k,],score[g,])
score=score %>% mutate(D_ORBP=c(ORBP_k,ORBP_g))
score=score %>% arrange(ScheduleKey)
ORBP_g=score$OR[g]/(score$OR[g]+score$DR[k])
ORBP_k=score$OR[k]/(score$OR[k]+score$DR[g])
score=rbind(score[k,],score[g,])
score=score %>% mutate(ORBP=c(ORBP_k,ORBP_g))
score=score %>% arrange(ScheduleKey)

#FourFatorsの相関係数を確認
library(corrplot)
score %>% select(eFG,TOP,FTR,ORBP) %>% cor(.) %>% corrplot.mixed(.)
score %>% select(D_eFG,D_TOP,D_FTP,D_ORBP) %>% cor(.) %>% corrplot.mixed(.)


#FourFatorsを標準化する
score=score %>% mutate(eFG_z=scale(eFG),TOP_z=scale(TOP),FTR_z=scale(FTR),ORBP_z=scale(ORBP))
score=score %>% mutate(D_eFG_z=scale(D_eFG),D_TOP_z=scale(D_TOP),D_FTP_z=scale(D_FTP),D_ORBP_z=scale(D_ORBP))

#千葉、宇都宮、川崎、琉球のデータを抽出
chiba=score %>% filter(NameShort=="千葉")
utsunomiya=score %>% filter(NameShort=="宇都宮")
kawasaki=score %>% filter(NameShort=="川崎")
ryukyu=score %>% filter(NameShort=="琉球")

#ダミー変数を使ってデータをまとめて分析する（記事で使った分析。攻撃側のFour Factorsのみ使用）
best4=rbind(chiba,utsunomiya,kawasaki,ryukyu)
best4$utsunomiya=ifelse(best4$NameShort=="宇都宮",1,0)
best4$kawasaki=ifelse(best4$NameShort=="川崎",1,0)
best4$ryukyu=ifelse(best4$NameShort=="琉球",1,0)

#4チームのFour Factorsの相関行列を確認
best4 %>% select(eFG,TOP,FTR,ORBP) %>% cor(.) %>% corrplot.mixed(.)

#ダミー変数を使ったロジスティック回帰
model=best4 %>% select(win,eFG_z,TOP_z,FTR_z,ORBP_z,utsunomiya,kawasaki,ryukyu) %>% 
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z+
        eFG_z:utsunomiya+TOP_z:utsunomiya+FTR_z:utsunomiya+ORBP_z:utsunomiya+
        eFG_z:kawasaki+TOP_z:kawasaki+FTR_z:kawasaki+ORBP_z:kawasaki+
        eFG_z:ryukyu+TOP_z:ryukyu+FTR_z:ryukyu+ORBP_z:ryukyu,
      data=.,family = binomial)

summary(model)

#ロジスティック回帰モデルを立てる
#攻撃と守備のFour Factorsの計8個の独立変数を投入するとモデルが収束しなかった。
#そのため、攻撃と守備で別々にロジスティック回帰する

#千葉の攻撃
model_chiba_off=chiba %>% 
  select(win,eFG_z,TOP_z,FTR_z,ORBP_z)%>%
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z,data=.,family = binomial)

#千葉の守備
model_chiba_def=chiba %>% 
  select(win,D_eFG_z,D_TOP_z,D_FTP_z,D_ORBP_z)%>%
  glm(win?D_eFG_z+D_TOP_z+D_FTP_z+D_ORBP_z,data=.,family = binomial)

summary(model_chiba_off)
summary(model_chiba_def)


#宇都宮の攻撃
model_utsunomiya_off=utsunomiya %>% 
  select(win,eFG_z,TOP_z,FTR_z,ORBP_z)%>%
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z,data=.,family = binomial)

#宇都宮の守備
model_utsunomiya_def=utsunomiya %>% 
  select(win,D_eFG_z,D_TOP_z,D_FTP_z,D_ORBP_z)%>%
  glm(win?D_eFG_z+D_TOP_z+D_FTP_z+D_ORBP_z,data=.,family = binomial)

summary(model_utsunomiya_off)
summary(model_utsunomiya_def)


#川崎の攻撃
model_kawasaki_off=kawasaki %>% 
  select(win,eFG_z,TOP_z,FTR_z,ORBP_z)%>%
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z,data=.,family = binomial)

#川崎の守備
model_kawasaki_def=kawasaki %>% 
  select(win,D_eFG_z,D_TOP_z,D_FTP_z,D_ORBP_z)%>%
  glm(win?D_eFG_z+D_TOP_z+D_FTP_z+D_ORBP_z,data=.,family = binomial)

summary(model_kawasaki_off)
summary(model_kawasaki_def)


#琉球の攻撃
model_ryukyu_off=ryukyu %>% 
  select(win,eFG_z,TOP_z,FTR_z,ORBP_z)%>%
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z,data=.,family = binomial)

#琉球の守備
model_ryukyu_def=ryukyu %>% 
  select(win,D_eFG_z,D_TOP_z,D_FTP_z,D_ORBP_z)%>%
  glm(win?D_eFG_z+D_TOP_z+D_FTP_z+D_ORBP_z,data=.,family = binomial)

summary(model_ryukyu_off)
summary(model_ryukyu_def)

#まとめて表示
summary(model_chiba_off)
summary(model_chiba_def)

summary(model_utsunomiya_off)
summary(model_utsunomiya_def)

summary(model_kawasaki_off)
summary(model_kawasaki_def)

summary(model_ryukyu_off)
summary(model_ryukyu_def)

#考察
#千葉、宇都宮は5%水準で有意な独立変数は攻守ともにeFGのみだった。
#川崎の攻撃では、eFGに加えてORBPも有意だった。守備はeFGのみ有意。
#琉球の攻撃ではeFG,FTR,ORBPが有意だった。守備ではeFG,TOP,ORPBの変数が有意だった。








