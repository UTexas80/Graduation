################################################################################
## Step 02.00: Vizualizaiton and interpretation                              ###
################################################################################
## Step 02.01: Eigenvalues / Variances                                       ###
################################################################################
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels   = TRUE, ylim = c(0, 45))
## -----------------------------------------------------------------------------
fviz_mca_biplot(res.mca,
               repel                = TRUE,
               ggtheme              = theme_minimal())
################################################################################
## Step 02.02: extract the results for variable categories                   ###
################################################################################
var <- get_mca_var(res.mca)
################################################################################
## Step 02.03: Correlation between variables and principal dimensions        ###
################################################################################
fviz_mca_var(res.mca, choice        = "mca.cor",
            repel                   = TRUE, # Avoid text overlapping (slow)
            ggtheme                 = theme_minimal())
################################################################################
## Step 02.04: Coordinates of variable categories                            ###
################################################################################
fviz_mca_var(res.mca,
             repel                  = TRUE, # Avoid text overlapping (slow)
             ggtheme                = theme_minimal())
## change the color and the shape of the variable points -----------------------
fviz_mca_var(res.mca,
             col.var                = "black",
             shape.var              = 15,
             repel                  = TRUE)
################################################################################
## Step 02.05: Quality of representation of variable categories              ###
## Color by cos2 values: quality on the factor map                           ###
################################################################################
head(var$cos2, 4)
## -----------------------------------------------------------------------------
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
## Change the transparency by cos2 values --------------------------------------
fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
## visualize the cos2 of row categories on all the dimensions-------------------
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.mca, choice = "var", axes = 1:2)
################################################################################
## Step 02.06: Contribution of variable categories to the dimensions         ###
################################################################################
head(round(var$contrib,2), 4)
## Contributions of rows to dimension 1 ----------------------------------------
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
## Contributions of rows to dimension 2 ----------------------------------------
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
## Total contribution to dimension 1 and 2 -------------------------------------
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
## -----------------------------------------------------------------------------
## The most important variable categories can be highlighted on the scatter plot
## -----------------------------------------------------------------------------
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
             )
## Change the transparency by contrib values -----------------------------------
fviz_mca_var(res.mca, alpha.var="contrib",
             repel = TRUE,
             ggtheme = theme_minimal())
################################################################################
## Step 02.07: Graph of individuals                                          ###
################################################################################
ind <- get_mca_ind(res.mca)
ind
## Plots: quality and contribution ---------------------------------------------
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
## ----------------------------------------------------------------------------
## bar plots of individuals cos2 and contributions
## ----------------------------------------------------------------------------
## Cos2 of individuals --------------------------------------------------------
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
## Contribution of individuals to the dimensions -------------------------------
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)
################################################################################
## Step 02.08: Color individuals by groups                                   ###
################################################################################
fviz_mca_ind(res.mca,
             label = "none", # hide individual labels
             habillage = "MINORITY", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())
################################################################################
## Step 02.09: Export plots to PDF/PNG files                                 ###
################################################################################
# Scree plot
scree.plot <- fviz_eig(res.mca)
# Biplot of row and column variables
biplot.mca <- fviz_mca_biplot(res.mca)
ggexport(plotlist = list(scree.plot, biplot.mca), 
         filename = "MCA.pdf")
################################################################################
## Step 02.99: VERSION HISTORY                                               ###
################################################################################
a02.version                         = "1.0.0"
a02.ModDate                         = as.Date("2020-01-27")
## -----------------------------------------------------------------------------
## 2020.01.27 - v.1.0.0
##  1st release
## -----------------------------------------------------------------------------
