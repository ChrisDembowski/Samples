
# You work for Motor Trend, a magazine about the automobile industry. Looking at
#	a data set of a collection of cars, they are interested in exploring the
#	relationship between a set of variables and miles per gallon (MPG)
#	(outcome). They are particularly interested in the following two questions:
#
# 		“Is an automatic or manual transmission better for MPG”
#
#		"Quantify the MPG difference between automatic and manual transmissions"
#




options( warn = -1)
library( ggplot2)
options( warn = 0)
library( gridExtra)
library( leaps)

data( mtcars)


Data = mtcars

Data$am[ Data$am == 1] = "man"

Data$am[ Data$am == 0] = "auto"

Data$vs[ Data$vs == 0] = "V"

Data$vs[ Data$vs == 1] = "straight"

#rename( Data, trans = am, config = vs)

names( Data) = c( "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "config", "trans", "gear", "carb")

Data$cyl = factor( Data$cyl)

Data$trans = factor( Data$trans)

Data$gear = factor( Data$gear)

Data$config = factor( Data$config)

Data$carb = factor( Data$carb)


# Exploratory analysis

plot.hp.disp = ggplot( Data, aes( x = disp, y = hp, colour = trans, label = rownames( Data)))

plot.hp.disp = plot.hp.disp + geom_point( size = 1)


plot.hp.cyl = ggplot( Data, aes( x = cyl, y = hp, colour = trans, label = rownames( Data)))

plot.hp.cyl = plot.hp.cyl + geom_point( size = 1)


plot.hp.wt = ggplot( Data, aes( x = wt, y = hp, colour = trans, label = rownames( Data)))

plot.hp.wt = plot.hp.wt + geom_point( size = 1)


plot.disp.cyl = ggplot( Data, aes( x = cyl, y = disp, colour = trans, label = rownames( Data)))

plot.disp.cyl = plot.disp.cyl + geom_point( size = 1)


grid.arrange( plot.hp.disp, plot.hp.cyl, plot.hp.wt, plot.disp.cyl, nrow = 2, ncol = 2, top = "Figure 1: HP vs. Disp, Cyl, and Wt; Disp vs. Cyl.")



plot.wt = ggplot( Data, aes( x = wt, y = mpg, colour = trans, label = rownames( Data)))

plot.wt = plot.wt + geom_point( size = 1)




plot.hp = ggplot( Data, aes( x = hp, y = mpg, colour = trans, label = rownames( Data)))

plot.hp = plot.hp + geom_point( size = 1)



plot.disp = ggplot( Data, aes( x = disp, y = mpg, colour = trans, label = rownames( Data)))

plot.disp = plot.disp + geom_point( size = 1)




plot.cyl = ggplot( Data, aes( x = cyl, y = mpg, colour = trans, label = rownames( Data)))

plot.cyl = plot.cyl + geom_point( size = 1)

grid.arrange( plot.wt, plot.hp, plot.disp, plot.cyl, nrow = 2, ncol = 2, top = "Figure 2: MPG vs. Wt, HP, Disp, and Cyl")



FULL.model = lm( mpg ~ ., Data)

summary( FULL.model)

MIN.model.trans = lm( mpg ~ trans, Data)
summary( MIN.model.trans)

MIN.model.disp = lm( mpg ~ disp, Data)
summary( MIN.model.disp)

MIN.model.hp = lm( mpg ~ hp, Data)
summary( MIN.model.hp)

MIN.model.wt = lm( mpg ~ wt, Data)
summary( MIN.model.wt)






# create all possible multiple variable subset models of
#	mpg ~ trans + cyl + disp + hp + wt

m1 = lm( mpg ~ trans + cyl + disp + hp + wt, Data)
m2 = lm( mpg ~ trans + cyl + disp + hp, Data)
m3 = lm( mpg ~ trans + cyl + disp + wt, Data)
m4 = lm( mpg ~ trans + cyl + hp + wt, Data)
m9 = lm( mpg ~ trans + disp + hp + wt, Data)
m16 = lm( mpg ~ cyl + disp + hp + wt, Data)
m5 = lm( mpg ~ trans + cyl + disp, Data)
m6 = lm( mpg ~ trans + cyl + hp, Data)
m7 = lm( mpg ~ trans + cyl + wt, Data)
m10 = lm( mpg ~ trans + disp + hp, Data)
m11 = lm( mpg ~ trans + disp + wt, Data)
m13 = lm( mpg ~ trans + hp + wt, Data)
m17 = lm( mpg ~ cyl + disp + hp, Data)
m18 = lm( mpg ~ cyl + disp + wt, Data)
m19 = lm( mpg ~ cyl + hp + wt, Data)
m23 = lm( mpg ~ disp + hp + wt, Data)
m8 = lm( mpg ~ trans + cyl, Data)
m12 = lm( mpg ~ trans + disp, Data)
m14 = lm( mpg ~ trans + hp, Data)
m15 = lm( mpg ~ trans + wt, Data)
m20 = lm( mpg ~ cyl + disp, Data)
m21 = lm( mpg ~ cyl + hp, Data)
m22 = lm( mpg ~ cyl + wt, Data)
m24 = lm( mpg ~ disp + hp, Data)
m25 = lm( mpg ~ disp + wt, Data)
m26 = lm( mpg ~ hp + wt, Data)



# save model summaries

s1 = summary( m1)
s2 = summary( m2)
s3 = summary( m3)
s4 = summary( m4)
s5 = summary( m5)
s6 = summary( m6)
s7 = summary( m7)
s8 = summary( m8)
s9 = summary( m9)
s10 = summary( m10)
s11 = summary( m11)
s12 = summary( m12)
s13 = summary( m13)
s14 = summary( m14)
s15 = summary( m15)
s16 = summary( m16)
s17 = summary( m17)
s18 = summary( m18)
s19 = summary( m19)
s20 = summary( m20)
s21 = summary( m21)
s22 = summary( m22)
s23 = summary( m23)
s24 = summary( m24)
s25 = summary( m25)
s26 = summary( m26)



# Obtain Adjusted R squared and PRESS values for each model
# Comments indicate number of predictors in each model

s1$adj.r.squared    	# 5
(pr1 = sum( (resid( m1) / (1 - hatvalues( m1))) ^ 2))

s2$adj.r.squared		# 4
(pr2 = sum( (resid( m2) / (1 - hatvalues( m2))) ^ 2))

s3$adj.r.squared		# 4
(pr3 = sum( (resid( m3) / (1 - hatvalues( m3))) ^ 2))

s4$adj.r.squared		# 4
(pr4 = sum( (resid( m4) / (1 - hatvalues( m4))) ^ 2))

s9$adj.r.squared		# 4
(pr9 = sum( (resid( m9) / (1 - hatvalues( m9))) ^ 2))

s16$adj.r.squared		# 4
(pr16 = sum( (resid( m16) / (1 - hatvalues( m16))) ^ 2))

s5$adj.r.squared		# 3
(pr5 = sum( (resid( m5) / (1 - hatvalues( m5))) ^ 2))

s6$adj.r.squared		# 3
(pr6 = sum( (resid( m6) / (1 - hatvalues( m6))) ^ 2))

s7$adj.r.squared		# 3
(pr7 = sum( (resid( m7) / (1 - hatvalues( m7))) ^ 2))

s10$adj.r.squared		# 3
(pr10 = sum( (resid( m10) / (1 - hatvalues( m10))) ^ 2))

s11$adj.r.squared		# 3
(pr11 = sum( (resid( m11) / (1 - hatvalues( m11))) ^ 2))

s13$adj.r.squared		# 3
(pr13 = sum( (resid( m13) / (1 - hatvalues( m13))) ^ 2))

s17$adj.r.squared		# 3
(pr17 = sum( (resid( m17) / (1 - hatvalues( m17))) ^ 2))

s18$adj.r.squared		# 3
(pr18 = sum( (resid( m18) / (1 - hatvalues( m18))) ^ 2))

s19$adj.r.squared		# 3
(pr19 = sum( (resid( m19) / (1 - hatvalues( m19))) ^ 2))

s23$adj.r.squared		# 3
(pr23 = sum( (resid( m23) / (1 - hatvalues( m23))) ^ 2))

s12$adj.r.squared		# 2
(pr12 = sum( (resid( m12) / (1 - hatvalues( m12))) ^ 2))

s14$adj.r.squared		# 2
(pr14 = sum( (resid( m14) / (1 - hatvalues( m14))) ^ 2))

s15$adj.r.squared		# 2
(pr15 = sum( (resid( m15) / (1 - hatvalues( m15))) ^ 2))

s20$adj.r.squared		# 2
(pr20 = sum( (resid( m20) / (1 - hatvalues( m20))) ^ 2))

s21$adj.r.squared		# 2
(pr21 = sum( (resid( m21) / (1 - hatvalues( m21))) ^ 2))

s22$adj.r.squared		# 2
(pr22 = sum( (resid( m22) / (1 - hatvalues( m22))) ^ 2))

s24$adj.r.squared		# 2
(pr24 = sum( (resid( m24) / (1 - hatvalues( m24))) ^ 2))

s25$adj.r.squared		# 2
(pr25 = sum( (resid( m25) / (1 - hatvalues( m25))) ^ 2))

s26$adj.r.squared		# 2
(pr26 = sum( (resid( m26) / (1 - hatvalues( m26))) ^ 2))





s1

s4

s16

s19

s22




# Perform nested likelihood ratio tests

anova( m22, m19, m4, m1)
anova( m22, m19, m16, m1)



# Build data fram to be used in table in report

adj.R.sq = c( s1$adj.r.squared, s4$adj.r.squared, s16$adj.r.squared, s19$adj.r.squared, s22$adj.r.squared)

PRESS = c( pr1, pr4, pr16, pr19, pr22)

model = c( "mpg = trans + cyl + disp + hp + wt", "mpg = trans + cyl + hp + wt", "mpg = cyl + disp + hp + wt", "mpg = cyl + hp + wt + ε", "mpg = cyl + wt")

model.table = data.frame( model, adj.R.sq, PRESS)

rownames( model.table) = c( "Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

colnames( model.table) = c( "Model", "Adjusted R²", "PRESS")




# Visually check candidate models

dev.new()
par( mfrow = c( 2, 2))
plot( m1, main = "Model 1")

dev.new()
par( mfrow = c( 2, 2))
plot( m4, main = "Model 2")

dev.new()
par( mfrow = c( 2, 2))
plot( m16, main = "Model 3")

dev.new()
par( mfrow = c( 2, 2))
plot( m19, main = "Model 4")

dev.new()
par( mfrow = c( 2, 2))
plot( m22, main = "Model 5")

