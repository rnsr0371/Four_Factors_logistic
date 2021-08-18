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
score=score %>% mutate(eFG=(F2GM+1.5*F3GM)/(F2GA+F3GA))
score=score %>% mutate(TOP=TO/(F2GA+F3GA+0.44*FTA+TO))
score=score %>% mutate(FTR=FTA/(F2GA+F3GA))
ORBP_g=score$OR[g]/(score$OR[g]+score$DR[k])
ORBP_k=score$OR[k]/(score$OR[k]+score$DR[g])
score=rbind(score[k,],score[g,])
score=score %>% mutate(ORBP=c(ORBP_k,ORBP_g))

#FourFatorsの相関係数を確認
library(corrplot)
score %>% select(win,eFG,TOP,FTR,ORBP) %>% cor(.) %>% corrplot.mixed(.)

#FourFatorsを標準化する
score=score %>% mutate(eFG_z=scale(eFG),TOP_z=scale(TOP),FTR_z=scale(FTR),ORBP_z=scale(ORBP))

#ロジスティック回帰モデルを立てる
model=score %>% select(win,eFG_z,TOP_z,FTR_z,ORBP_z) %>% glm(win?eFG_z+TOP_z+FTR_z+ORBP_z,data=.,family = binomial)
summary(model)

#モデルで予測を立てる
score=score %>% mutate(percentage=predict(model,type="response"))
score=score %>% mutate(pred=ifelse(percentage>0.5,1,0))

#予測精度の確認（正答率）
sum(score$pred==score$win)/nrow(score)


