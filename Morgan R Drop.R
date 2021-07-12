# Morgan's r drop attempt
# Naming convention:
# a.z1 = First factor, second factor, first permutation
# a = Affective
# b = Behavioral
# c = Cognitive
# v = vigor
# d = dedication
# a = absorbtion

# I completely atheoretically go through each 2-item combination within the set of three items for each bifactor pair

#Affective absorbtion
a.a1 <- psych::alpha(together[,c(2,3)])
a.a2 <- psych::alpha(together[,c(2,5)])
a.a3 <- psych::alpha(together[,c(3,5)])


#Affective vigor
a.v1 <- psych::alpha(together[,c(6,7)])
a.v2 <- psych::alpha(together[,c(6,8)])
a.v3 <- psych::alpha(together[,c(7,8)])


#Affective dedication
a.d1 <- psych::alpha(together[,c(10,12)])
a.d2 <- psych::alpha(together[,c(10,13)])
a.d3 <- psych::alpha(together[,c(12,13)])



#behavioral absorbtion
b.a1 <- psych::alpha(together[,c(14,15)])
b.a2 <- psych::alpha(together[,c(14,17)])
b.a3 <- psych::alpha(together[,c(15,17)])


#behavioral vigor
b.v1 <- psych::alpha(together[,c(18,19)])
b.v2 <- psych::alpha(together[,c(18,21)])
b.v3 <- psych::alpha(together[,c(19,21)])


#behavioral dedication
b.d1 <- psych::alpha(together[,c(23,24)])
b.d2 <- psych::alpha(together[,c(23,25)])
b.d3 <- psych::alpha(together[,c(24,25)])



#cognitive absorbtion
c.a1 <- psych::alpha(together[,c(34,36)])
c.a2 <- psych::alpha(together[,c(34,37)])
c.a3 <- psych::alpha(together[,c(36,37)])


#cognitive vigor
c.v1 <- psych::alpha(together[,c(26,27)])
c.v2 <- psych::alpha(together[,c(26,29)])
c.v3 <- psych::alpha(together[,c(27,29)])


#cognitive dedication
c.d1 <- psych::alpha(together[,c(31,32)])
c.d2 <- psych::alpha(together[,c(31,33)])
c.d3 <- psych::alpha(together[,c(32,33)])



a.a1 #alpha = .53
a.a2 #0.68
a.a3 #0.45

a.v1 #0.70
a.v2 #0.68
a.v3 #0.60

a.d1 #0.68
a.d2 #0.77
a.d3 #0.71

b.a1 #0.56
b.a2 #0.45
b.a3 #0.39

b.v1 #0.60
b.v2 #0.49
b.v3 #0.50

b.d1 #0.50
b.d2 #0.37
b.d3 #0.62

c.a1 #0.58
c.a2 #-.0052 wow, definitely don't use this
c.a3 #0.16

c.v1 #0.24
c.v2 #0.65
c.v3 #0.62

c.d1 #0.70
c.d2 #0.80
c.d3 #0.64
















