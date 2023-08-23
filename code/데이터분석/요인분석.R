df = read.csv('/Users/seoihwan/Desktop/escuela/문화관광빅데이터/pow-gongmo/data/final/final_animal_visit_travel_검색건수_eng.csv')
head(df)
df = read.csv('/Users/seoihwan/Desktop/escuela/문화관광빅데이터/문스4.csv')
# 결측치 확인
colSums(is.na(df))
df = df[,-9]
df
str(df)
library(psych)
library(nFactors)
fa.parallel(df, fm='ml',fa='fa',n.iter=100)
nScree(df)
eigen(cor(df))

# 요인 2,3개 만들기
pc2 = principal(df, nfactors=2, rotate='varimax')
pc3 = principal(df, nfactors=3, rotate='varimax')
pc4 = principal(df, nfactors=4, rotate='varimax')
print.psych(pc3, sort=T)
print.psych(pc4, sort=T)
print.psych(pc2, cut = .3, sort=T)
summary(pc3)
# 요인분석 적절한지 적합성검정
install.packages('REdaS')
library('REdaS')
kmos = KMOS(df)
kmos
cortest.bartlett(df)

install.packages('semPlot')
library('semPlot')
semPaths(pc2, what='est',residuals=F,cut=.3,posCol=c('white','darkgreen'),
         negCol=c('white','red'),edge.label.cex=.75)
semPaths(pc3, what='est',residuals=F,cut=.5,posCol=c('white','darkgreen'),
         negCol=c('white','red'),edge.label.cex=.75)
semPaths(pc4, what='est',residuals=F,cut=.5,posCol=c('white','darkgreen'),
         negCol=c('white','red'),edge.label.cex=.75)
write.csv(pc3$scores,'FA.csv',row.names = F)


