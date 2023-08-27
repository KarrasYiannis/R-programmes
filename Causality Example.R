#https://datadryad.org/stash/dataset/doi:10.5061/dryad.6m905qg22
#https://www.seascapemodels.org/structural-causal-models-tutorial/scm-tute.html

install.packages(c("dagitty","tidyverse","ggdag"
                   ,"visreg","patchwork"))

library(ggplot2)
library(patchwork)
library(ggdag)

library(dagitty)
theme_set(theme_dag())

king_mod <- dagify(lncelld ~ PI + Diuron,
                  PI ~ Light +Diuron)

g1 <- ggdag(king_mod, text_size = 2,
            node_size = 12);g1

library(piecewiseSEM)

king_psem <- psem(
  lm(Lncelld ~ PI + Diuron, data = king)
  lm(PI ~ Diuron + Light,data = king)
)

summary(king_psem)
