######################################
# Latitude (lamby) INTERACTION PLOT  #
######################################

# Moderators
lambx.high = with(subdata, max(lambx))
lambx.low  = with(subdata, min(lambx))
month.high = 1 # June
month.low  = 1 # February
elev.high  = with(subdata, max(ELEVATION))
elev.low   = with(subdata, min(ELEVATION))

# Getting coefficients from regression model



# Creating Slopes

lxH.june.elevH  = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lambx.high
lxL.june.elevH  = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lambx.low
lxH.feb.elevH   =  lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lambx.high
lxL.feb.elevH   = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)2"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lambx.low
lxH.june.elevL  = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lambx.high
lxL.june.elevL  = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lambx.low
lxH.feb.elevL   = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lambx.high
lxL.feb.elevL   = lmfit$coeff["lamby"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lambx.low
    
    
i.lxH.june.elevH = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.high +
     lmfit$coeff["lambx:as.factor(month)6"]*lambx.high*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high + lmfit$coeff["lambx:ELEVATION"]*lambx.high*elev.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lxL.june.elevH = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.low +
     lmfit$coeff["lambx:as.factor(month)6"]*lambx.low*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high + lmfit$coeff["lambx:ELEVATION"]*lambx.low*elev.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lxH.feb.elevH  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.high +
     lmfit$coeff["lambx:as.factor(month)2"]*lambx.high*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low + lmfit$coeff["lambx:ELEVATION"]*lambx.high*elev.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lxL.feb.elevH  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.low +
     lmfit$coeff["lambx:as.factor(month)2"]*lambx.low*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low + lmfit$coeff["lambx:ELEVATION"]*lambx.low*elev.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lxH.june.elevL = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.high +
     lmfit$coeff["lambx:as.factor(month)6"]*lambx.high*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high + lmfit$coeff["lambx:ELEVATION"]*lambx.high*elev.low +
     lmfit$coeff["ELEVATION"]*elev.low
i.lxL.june.elevL = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.low +
     lmfit$coeff["lambx:as.factor(month)6"]*lambx.low*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high + lmfit$coeff["lambx:ELEVATION"]*lambx.low*elev.low +
     lmfit$coeff["ELEVATION"]*elev.low
i.lxH.feb.elevL  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.high +
     lmfit$coeff["lambx:as.factor(month)2"]*lambx.high*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low + lmfit$coeff["lambx:ELEVATION"]*lambx.high*elev.low +
     lmfit$coeff["ELEVATION"]*elev.low
i.lxL.feb.elevL  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.low +
     lmfit$coeff["lambx:as.factor(month)2"]*lambx.low*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low + lmfit$coeff["lambx:ELEVATION"]*lambx.low*elev.low +
     lmfit$coeff["ELEVATION"]*elev.low



ly0 = with(subdata,seq(min(lamby), max(lamby), length.out = 2))

PredTemp = c(i.lxH.june.elevL + lxH.june.elevL*ly0,
i.lxL.june.elevL + lxL.june.elevL*ly0,
i.lxH.june.elevH + lxH.june.elevH*ly0,
i.lxL.june.elevH + lxL.june.elevH*ly0,

i.lxL.feb.elevL + lxL.feb.elevL*ly0,
i.lxH.feb.elevL + lxH.feb.elevL*ly0,
i.lxL.feb.elevH + lxL.feb.elevH*ly0,
i.lxH.feb.elevH + lxH.feb.elevH*ly0)



summary(PredTemp)

lydf = data.frame(x = rep(ly0, times = 8),PredTemp = PredTemp, type =  gl(8,2))

interactionPlot = ggplot(lydf, aes(x =  x, y = PredTemp)) +
    geom_line(aes(colour = factor(type)), size = 1.5) + theme_bw() + xlab(expression("Centred Latitude (" * km %*% 10^-2 * ")")) +
    ylab(expression("Temperature (" * degree * C *")")) +
    scale_colour_manual(values = brewer.pal(n = 10, name = 'Spectral')[-c(5:6)],
    #rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")),
                      name = "Scenarios",
                      labels = c("Eastern, Low, June",
                                 "Western, Low, June",
                                 "Eastern, High, June",
                                 "Western, High, June",
                                 "Western, Low, Feb" ,
                                 "Eastern, Low, Feb",
                                 "Western, High, Feb",
                                 "Eastern, High, Feb"))


ggsave("interactionPlotLat.pdf")


################################################################################


######################################
# Latitude (lamby) INTERACTION PLOT  #
######################################

# Moderators
lambx.high = with(subdata, max(lambx))
lambx.low  = with(subdata, min(lambx))
month.high = 1 # June
month.low  = 1 # February
elev.high  = with(subdata, max(ELEVATION))
elev.low   = with(subdata, min(ELEVATION))
lamby.high = with(subdata, max(lamby))
lamby.low  = with(subdata, min(lamby))

# Getting coefficients from regression model



# Creating Slopes

lyH.june.elevH  = lmfit$coeff["lambx"] + lmfit$coeff["lambx:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lamby.high + lmfit$coeff["ELEVATION"]*elev.high
lyL.june.elevH  = lmfit$coeff["lambx"] + lmfit$coeff["lambx:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lamby.low + lmfit$coeff["ELEVATION"]*elev.high
lyH.feb.elevH   =  lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lamby.high + lmfit$coeff["ELEVATION"]*elev.high
lyL.feb.elevH   = lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)2"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lamby.low + lmfit$coeff["ELEVATION"]*elev.high
lyH.june.elevL  = lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lamby.high + lmfit$coeff["ELEVATION"]*elev.low
lyL.june.elevL  = lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)6"]*month.high +
    lmfit$coeff["lambx:lamby:as.factor(month)6"]*month.high*lamby.low + lmfit$coeff["ELEVATION"]*elev.low
lyH.feb.elevL   = lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lamby.high + lmfit$coeff["ELEVATION"]*elev.low
lyL.feb.elevL   = lmfit$coeff["lambx"] + lmfit$coeff["lamby:as.factor(month)2"]*month.low +
    lmfit$coeff["lambx:lamby:as.factor(month)2"]*month.low*lamby.low + lmfit$coeff["ELEVATION"]*elev.low


i.lyH.june.elevH = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.high +
     lmfit$coeff["lamby:as.factor(month)6"]*lamby.high*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lyL.june.elevH = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.low +
     lmfit$coeff["lamby:as.factor(month)6"]*lamby.low*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high +
     lmfit$coeff["ELEVATION"]*elev.high
i.lyH.feb.elevH  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.high +
     lmfit$coeff["lamby:as.factor(month)2"]*lamby.high*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low +
     lmfit$coeff["ELEVATION"]*elev.high
i.lyL.feb.elevH  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.low +
     lmfit$coeff["lamby:as.factor(month)2"]*lamby.low*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low +
     lmfit$coeff["ELEVATION"]*elev.high
i.lyH.june.elevL = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.high +
     lmfit$coeff["lamby:as.factor(month)6"]*lamby.high*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high +
     lmfit$coeff["ELEVATION"]*elev.low
i.lyL.june.elevL = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.low +
     lmfit$coeff["lamby:as.factor(month)6"]*lamby.low*month.high +
     lmfit$coeff["as.factor(month)6"]*month.high +
     lmfit$coeff["ELEVATION"]*elev.low
i.lyH.feb.elevL  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lamby"]*lamby.high +
     lmfit$coeff["lamby:as.factor(month)2"]*lamby.high*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low +
     lmfit$coeff["ELEVATION"]*elev.low
i.lyL.feb.elevL  = lmfit$coeff["(Intercept)"] + lmfit$coeff["lambx"]*lambx.low +
     lmfit$coeff["lambx:as.factor(month)2"]*lambx.low*month.low +
     lmfit$coeff["as.factor(month)2"]*month.low + lmfit$coeff["lambx:ELEVATION"]*lambx.low*elev.low +
     lmfit$coeff["ELEVATION"]*elev.low



lx0 = with(subdata,seq(min(lambx), max(lambx), length.out = 2))

PredTemp2 = c(i.lyH.june.elevL + lyH.june.elevL*lx0,
i.lyL.june.elevL + lyL.june.elevL*lx0,
i.lyH.june.elevH + lyH.june.elevH*lx0,
i.lyL.june.elevH + lyL.june.elevH*lx0,
i.lyL.feb.elevL + lyL.feb.elevL*lx0,
i.lyH.feb.elevL + lyH.feb.elevL*lx0,
i.lyL.feb.elevH + lyL.feb.elevH*lx0,
i.lyH.feb.elevH + lyH.feb.elevH*lx0)

lxdf = data.frame(x = rep(lx0, times = 8),PredTemp = PredTemp2, type =  gl(8,2))

ggplot(lxdf, aes(x =  x, y = PredTemp)) +
    geom_line(aes(colour = factor(type)), size = 1.5) + theme_bw() + xlab(expression("Centred Longitude (" * km %*% 10^-2 * ")")) +
    ylab(expression("Temperature (" * degree * C *")")) +
    scale_colour_manual(values = brewer.pal(n = 10, name = 'Spectral')[-c(5:6)],
    #rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")),
                      name = "Scenarios",
                      labels = c("Northern, Low, June",
                                 "Southern, Low, June",
                                 "Northern, High, June",
                                 "Southern, High, June",
                                 "Southern, Low, Feb" ,
                                 "Northern, Low, Feb",
                                 "Southern, High, Feb",
                                 "Northern, High, Feb"))


ggsave("interactionPlot.pdf")