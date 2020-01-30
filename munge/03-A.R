# Multiple correspondence analysis (MCA) is an extension of correspondenceanalysis(CA)
# whichallowsonetoanalyzethepatternof relationships of several categorical dependent variables.

net_worth <- cbind(graduation[74], dt_graduation)
colnames(net_worth)[1] <- "net_worth"
viz_net_worth <- MCA(net_worth[c(1)>0], ncp = 2, quanti.sup=1,quali.sup=10, graph = TRUE)
print(viz_net_worth)

awd_bdgt <- cbind(graduation[15], dt_graduation)
colnames(awd_bdgt)[1] <- "award_budget"
viz_awd_bdgt <- MCA(awd_bdgt[c(1)>0],  ncp = 2, quanti.sup=1,quali.sup=10, graph = TRUE)
print(viz_awd_bdgt)
