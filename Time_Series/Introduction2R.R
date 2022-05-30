data(coagulation, package='faraway')
ls()
coagulation
plot(coag~diet, data =coagulation)

# 2nd example
data(worldcup, package='faraway')
ls()
worldcup
mean(worldcup$Time)
