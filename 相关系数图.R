#两个变量的相关性分析
#参数相关性检验：皮尔逊相关性分析/参数检验
#去掉组分变量
data <- mtcars[,c(1,3,4,5,6,7)]
#相关系数矩阵
cor(data, method='pearson')
#rcorr(data)
#cor计算相关关系，但是样本是从总体中抽取的一部分数据，所以计算的相关
#系数不一定能代表总体数据的相关系数p，因此需要显著性检验，计算显著性水平
#p>0.05, 碰巧出现的可能性大于5%，不能否定原假设，两组差别无显著差异
#p<0.05, 碰巧出现的可能性小于5%，可以否定原假设，两组差别有显著意义
#p<0.01, 碰巧出现的可能性小于1%， 可以否定原假设，两组差异极显著


#两个矩阵相关关系的检验， mental test

#ggcor 拓展包
#cor
#cor.test匹配对之间的相关性检验
#或者
#处理数据后用as_cor_tbl()和fortify_cor()两个函数导入
#几个重要的图层
#geom_square, geom_circle2(), geom_ellipse2(), geom_pie2()
#geom_colour(), geom_confbox(), geom_num(), geom_mark(), geom_cross()
#参数
#r 相关系数值，数值会直接显示在图标上，
#p 相关系数检验p值，适用于mark， cross，结合sig.thres 根据显著性水平做一些辅助标记
#r low upp适用confbox，分别确定置信区间盒子的中间、下端、上端线条位置


#非映射参数，
#r0 外接圆半径缩放系数，
#sig_thres  - P.value临界值，规定可以显示的显著性阈值
#sig.levle mark 前者为统计显著性水平向量，后者为对应的标记符号
#例c(0.001,0.01,0.05); c("***","**","*")
#此时相关矩阵的图就会根据阈值，添加标记符号，注意vjust

#corr.test(){psych}可以自动对多重比较结果进行P值矫正
require(psych)
require(ggcor)
#install.packages("ggcor")


#method_1
if(!require(devtools))
  install.packages("devtool")
if(!require(ggcor)) {
  devtools::install_github("houyunhuang/ggcor")
}

#method_2
install.packages("remotes")
remotes::install_github("houyunhuang/ggcor")


require(ggcor)
corr <- cor(mtcars)
#将相关系数矩阵转化为表的形式，两列为相关对象，第三列为相关系数
df <- as_cor_tbl(corr)

#fortify_cor()主要适用于处理原始数据表，即调用cor()求相关系数
#cor.test 决定是否进行相关性检验，cluster.type是否对相关系数矩阵进行重新排序，"none"不重排，“all”表示行列均重排
df01 <- fortify_cor(mtcars, cor.test = TRUE, cluster.type = "all")


ggcor(mtcars)
ggcor(mtcars, type = "lower")
df02 <- fortify_cor(mtcars, type="upper")
ggcor(df02, panel.backgroud = "#66C2A5")
ggcor(mtcars) + geom_square()
ggcor(mtcars, type = "upper") + geom_circle2()
ggcor(mtcars, type = "lower", show.diag = TRUE) + geom_ellipse2()
ggcor(mtcars, type = "full", cluster.type = 'all') + geom_pie2()

ggcor(mtcars, cluster.type = "all")+
  geom_colour()+
  geom_num(aes(num=r), colour = "grey90", size=3.5)
ggcor(mtcars, type = "full", cor.test = TRUE) + geom_confbox()
ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_colour() + geom_cross()
ggcor(mtcars, type = "full", cor.test = TRUE) +
  geom_square() + geom_cross()

ggcor(mtcars, type = "upper", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark(sig.thres = 0.05, size = 3, colour = "grey90")


ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark(r = NA, sig.thres = 0.05, size = 5, colour = "grey90")

####mark
ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark( sig.level = c(0.05,0.01,0.001), mark = c("^^^","^^","^"), size = 3, colour = "grey90")

library(vegan) # 使用vegan包所带的数据集
data(varechem)
data(varespec)

df05 <- fortify_cor(x = varechem, cor.test = TRUE, cluster.type = "all")
ggcor(df05) + geom_circle2()
dt <- fortify_cor(x=mtcars, cor.test = TRUE, cluster.type = "all")
ggcor(dt) + geom_circle2()
df05_lower <- get_lower_data(df05, show.diag = FALSE)
ggcor(df05_lower) + geom_circle2()

ggcor(dt) +
  geom_pie2(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_ellipse2(data = get_data(type = "lower", show.diag = TRUE))


ggcor(df05) +
  geom_segment(aes(x = x - 0.5, y = y + 0.5, xend = x + 0.5, yend = y - 0.5),
               data = get_data(type = "diag"), size = 0.5, colour = "grey60") +
  geom_colour(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 3) +
  geom_circle2(data = get_data(r >= 0.5, type = "lower", show.diag = FALSE),
               r = 0.8, fill = "#66C2A5") +
  geom_num(aes(num = r), data = get_data(type = "lower",
                                         show.diag = FALSE), size = 3)


ggcor(mtcars, cor.test = TRUE, cluster.type = "all") +
  geom_pie2(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_num(aes(num = r), data = get_data(type = "lower", show.diag = FALSE), size = 3.5) +
  #geom_mark( sig.level = c(0.05,0.01,0.001), mark = c("^^^","^^","^"), size = 3, colour = "grey90")+
  add_diaglab(size = 4.56) + remove_axis()


ggcor(mtcars, cor.test = TRUE, cluster.type = "all") +
  geom_num(aes(num = r), data = get_data(type = "lower", show.diag = FALSE), size = 3.5) +
  geom_mark( sig.level = c(0.05,0.01,0.001), mark = c("^^^","^^","^"), size = 3, colour = "grey90")+
  add_diaglab(size = 4.56) + remove_axis()
ggcor(mtcars, cor.test = TRUE, cluster.type = "all") +
  geom_confbox(data = get_data(type = "upper", show.diag = FALSE)) #+
  geom_num(aes(num = r), data = get_data(type = "lower", show.diag = FALSE), size = 3.5) +
  add_diaglab(size = 4.56) + remove_axis()


dt <- fortify_cor(x=mtcars, cor.test = TRUE, cluster.type = "all")
ggcor(dt) +
  #geom_segment(aes(x = x - 0.5, y = y + 0.5, xend = x + 0.5, yend = y - 0.5),
               #data = get_data(type = "diag"), size = 0.5, colour = "grey60") #+
  geom_colour(data = get_data(type = "lower", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "lower", show.diag = FALSE), size = 4) +
  #geom_circle2(data = get_data(r >= 0.5, type = "lower", show.diag = FALSE),
               #r = 0.8, fill = "#66C2A5") +
  #geom_num(aes(num = r), data = get_data(type = "lower",
                                        # show.diag = FALSE), size = 3)+
  geom_pie2(data = get_data(type = "upper", show.diag = FALSE))+
  add_diaglab(size = 5, color = "red") + 
  remove_axis()













#表情
#library(ggimage)
if(!require(ggimage)){
  install.packages("ggimage")
}
emoji <- c("1f004", "1f0cf", "1f170", "1f171", "1f17e",
           "1f17f", "1f18e", "1f191", "1f192", "1f193",
           "1f194", "1f195", "1f196", "1f197")
ggcor(df05) +
  geom_pokemon(aes(image=ifelse(r > 0.5, 'pikachu', 'tauros')),
               data = get_data(type = "lower", show.diag = FALSE)) +
  geom_emoji(aes(image = ifelse(p <= 0.05, '1f600', '1f622')),
             data = get_data(type = "upper", show.diag = FALSE)) +
  geom_emoji(aes(image = emoji), data = get_data(type = "diag"))
ggcor(df05) +
  geom_pokemon(aes(image=ifelse(r > 0.5, 'pikachu', 'tauros')),
               data = get_data(type = "lower", show.diag = FALSE)) +
  geom_colour(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_shade(data = get_data(type = "upper", show.diag = FALSE),
             sign = -1, size = 0.1) +
  geom_emoji(aes(image = emoji), data = get_data(type = "diag"))
