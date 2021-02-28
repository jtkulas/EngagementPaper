# R Drops

aff.abs <- psych::alpha(together[,c(2,3,5)])
aff.vig <- psych::alpha(together[6:8])
aff.ded <- psych::alpha(together[,c(10,12,13)])

beh.abs <- psych::alpha(together[,c(14,15,17)])
beh.vig <- psych::alpha(together[,c(18,19,21)])
<<<<<<< HEAD
beh.ded <- psych::alpha(together[,c(23:25)])
=======
beh.ded <- psych::alpha(together[22:25])
>>>>>>> e4cee70a0d3a4b12785d98fbbe9b17ea5593cfff

cog.abs <- psych::alpha(together[,c(34,36,37)])
cog.vig <- psych::alpha(together[,c(26,27,29)])
cog.ded <- psych::alpha(together[31:33])


aff.abs
aff.vig
aff.ded

beh.abs
beh.vig
<<<<<<< HEAD
beh.ded

cog.abs
cog.vig
cog.ded
=======
>>>>>>> e4cee70a0d3a4b12785d98fbbe9b17ea5593cfff
