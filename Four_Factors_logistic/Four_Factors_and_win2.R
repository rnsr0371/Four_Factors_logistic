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
score=score %>% mutate(eFG=(F2GM+1.5*F3GM)/(F2GA+F3GA))

#TO%
score=score %>% mutate(TOP=TO/(F2GA+F3GA+0.44*FTA+TO))

#FT%
score=score %>% mutate(FTR=FTA/(F2GA+F3GA))

#ORBP
ORBP_g=score$OR[g]/(score$OR[g]+score$DR[k])
ORBP_k=score$OR[k]/(score$OR[k]+score$DR[g])
score=rbind(score[k,],score[g,])
score=score %>% mutate(ORBP=c(ORBP_k,ORBP_g))
score=score %>% arrange(ScheduleKey)

#千葉・宇都宮・川崎・琉球のデータだけを抜き出す
chiba=score %>% filter(NameShort=="千葉")
utsunomiya=score %>% filter(NameShort=="宇都宮")
kawasaki=score %>% filter(NameShort=="川崎")
ryukyu=score %>% filter(NameShort=="琉球")
best4=rbind(chiba,utsunomiya,kawasaki,ryukyu)

#相関行列の確認
library(corrplot)
best4 %>% select(win,eFG,TOP,FTR,ORBP) %>% cor(.) %>% corrplot.mixed()

#Four Factorsを標準化する
best4=best4 %>% mutate(eFG_z=scale(eFG),TOP_z=scale(TOP),FTR_z=scale(FTR),ORBP_z=scale(ORBP))

#ダミー変数の作成
best4$utsunomiya=ifelse(best4$NameShort=="宇都宮",1,0)
best4$kawasaki=ifelse(best4$NameShort=="川崎",1,0)
best4$ryukyu=ifelse(best4$NameShort=="琉球",1,0)

#ロジスティック回帰
model=best4 %>% select(win,eFG_z,TOP_z,FTR_z,ORBP_z,utsunomiya,kawasaki,ryukyu) %>% 
  glm(win?eFG_z+TOP_z+FTR_z+ORBP_z+
        eFG_z:utsunomiya+TOP_z:utsunomiya+FTR_z:utsunomiya+ORBP_z:utsunomiya+
        eFG_z:kawasaki+TOP_z:kawasaki+FTR_z:kawasaki+ORBP_z:kawasaki+
        eFG_z:ryukyu+TOP_z:ryukyu+FTR_z:ryukyu+ORBP_z:ryukyu,
      data=.,family = binomial)

summary(model)
