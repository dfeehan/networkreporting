# Suppress R CMD check notes about non-standard evaluation variable bindings.
# These variables are used as column names in dplyr operations.
utils::globalVariables(c(
  ":=", ".", ".age.offset", ".agecat", ".ego.id", ".ego.weight", ".end.obs", ".event",
  ".id", ".ind_vis_weight", ".sib.id", ".sib.in.F", ".start.obs", ".time.offset",
  ".weight", ".y.F", "age", "age.cat", "agegroup", "agelabel", "asdr.hat", "boot_idx",
  "cell", "denom.hat", "ind_vis", "ind.denom.ego", "ind.num.ego", "N.Falpha",
  "N.Fminusalpha", "normalized_diff", "num.hat", "occ", "qty", "rawqty", "sex",
  "sib.exp", "sib.occ", "sib.size", "time.period", "total", "value", "wwgt", "y.Dcell",
  "y.Dcell.ind", "y.F", "y.F.bar", "y.Falpha.Fminusalpha", "y.Fminusalpha.Falpha",
  "y.NandFcell", "y.Ncell", "y.Ncell.ind",
  ".S.hat", ".donor.S", ".donor.w", ".rule", "n_alters", "n_donors", "share", "vis", "vis_rule", "vis_weight", "y.DandFcell", "y.DandnotFcell", "y.NandnotFcell", "yprime.F"
))
