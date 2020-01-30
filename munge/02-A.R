################################################################################
## Step 02.00: Vizualizaiton and interpretation                              ###
################################################################################
## Step 02.01: Eigenvalues / Variances                                       ###
################################################################################
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels   = TRUE, ylim = c(0, 45))
################################################################################
## Step 02.01a: Biplot                                                       ###
################################################################################
# fviz_mca_biplot(res.mca,
#                repel                = TRUE,
#                ggtheme              = theme_minimal())
################################################################################
## Step 02.02: extract the results for variable categories                   ###
################################################################################
var <- get_mca_var(res.mca)
################################################################################
## Step 02.03: Correlation between variables and principal dimensions        ###
## The plot above helps to identify variables that are the most correlated   ###
## with each dimension.                                                      ###
################################################################################
viz_var <- fviz_mca_var(res.mca, choice        = "mca.cor",
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
## The quality of the representation is called the squared cosine (cos2),    ###
## which measures the degree of association between variable categories and  ###
## a particular axis. If a variable category is well represented by two      ###
## dimensions, the sum of the cos2 is closed to one.                         ###
################################################################################
quality_of_representation <- head(var$cos2, 4)
## -----------------------------------------------------------------------------
viz_var_cat_co2 <- fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
## Change the transparency by cos2 values --------------------------------------
fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
## visualize the cos2 of row categories on all the dimensions-------------------
viz_corrplot <- corrplot(var$cos2, is.corr=FALSE)
viz_dim_co2  <- fviz_cos2(res.mca, choice = "var", axes = 1:2)
################################################################################
## Step 02.06: Contribution of variable categories to the dimensions         ###
################################################################################
head(round(var$contrib,2), 4)
## Contributions of rows to dimension 1 ----------------------------------------
viz_dim1_co2  <- fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
## Contributions of rows to dimension 2 ----------------------------------------
viz_dim2_co2  <- fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
## Total contribution to dimension 1 and 2 -------------------------------------
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
## -----------------------------------------------------------------------------
## The most important variable categories can be highlighted on the scatter plot
## The plot above gives an idea of what pole of the dimensions the categories
## are actually contributing to.
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
dt_ind_quality <- data.table((ind[[2]]))
dt_ind_contrib <- data.table((ind[[3]]))
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
viz_dim_ind <- fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)
################################################################################
## Step 02.08: Color individuals by groups                                   ###
################################################################################
fviz_mca_ind(res.mca,
             label = "none", # hide individual labels
             habillage = 1, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())
viz_factor_mapA <- fviz_ellipses(res.mca, 1:4, geom = "point")
viz_factor_mapB <- fviz_ellipses(res.mca, 5:8, geom = "point")
viz_factor_mapC <- fviz_ellipses(res.mca, 9:10, geom = "point")
################################################################################
## Step 02.089 Filtering Results                                            ###
## Top 2 individuals; Top 5 Categories          -------------------------------
################################################################################

viz_top_Results <-  fviz_mca_biplot(res.mca, select.ind = list(contrib = 2),
                select.var = list(contrib = 5),
                ggtheme = theme_minimal())

################################################################################
## Step 02.09: Export plots to PDF/PNG files                                 ###
################################################################################
# Scree plot
scree.plot <- fviz_eig(res.mca)
# Biplot of row and column variables
# top_Results <- fviz_mca_biplot(res.mca)
ggexport(plotlist = list(
  scree.plot,
  viz_var,
  viz_var_cat_co2,
  viz_corrplot,
  viz_dim1_co2,
  viz_dim2_co2,
  viz_dim_co2,
  viz_dim_ind,
  viz_factor_mapA,
  viz_factor_mapB,
  viz_factor_mapC,
  viz_top_Results),
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
