# R Drops

aff.abs <- psych::alpha(together[,c(2,3,5)])
aff.vig <- psych::alpha(together[6:8])
aff.ded <- psych::alpha(together[,c(10,12,13)])

beh.abs <- psych::alpha(together[,c(14,15,17)])
beh.vig <- psych::alpha(together[,c(18,19,21)])

beh.ded <- psych::alpha(together[,c(23:25)])

cog.abs <- psych::alpha(together[,c(34,36,37)])
cog.vig <- psych::alpha(together[,c(26,27,29)])
cog.ded <- psych::alpha(together[31:33])


aff.abs
aff.vig
aff.ded

beh.abs
beh.vig
beh.ded

cog.abs
cog.vig
cog.ded


