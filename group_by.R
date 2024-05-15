### troubleshooting how to "group by"
# We want to be able to group by statistic or phenotypic category
# We'll focus on statistic first...

# We have a dataframe called phen_study that contains things like the study 
# name, dataset, map type, stat type, phenotypic code, and parcellation

# We also have a list of lists called d, where each entry is a study with n, 
# d, CI lb, CI ub. The names of each list in d are the same as the name column
# of the phen_study dataframe.

# phen_study and d both get filtered in the Shiny app based on input parameters
# from the user, so that at any given point, v$phen_study and v$d contain 
# only the studies that fit the selected parameters.

# if the user selects group_by = "statistic", we want the data to be averaged
# across statistic type and then plotted... so we would have one plot that is 
# the average of all t studies, one for the average of all t2 studies, etc.

# that seems simple and there are many ways to accomplish this.
# I could just make an index for each stat type that is true for that stat type,
# and then use that index to select those study names that fit, and from those
# names then get their d values and average studies to get average d for each
# and then plot each of those...

# ABCD_fc_r_rest_age
# edge: 1 2 3 4 5 ... 35778
# d:
# ci_lb:
# ci_ub: 
# 
# ABCD_fc_r_rest_bmi
# edge: 1 2 3 4 5 ... 35778
#
# UKB_fc_r_rest_gf
# edge: 1 2 3 4 5 ... 1485 !!!

# BUT here are a few problems with that:
# 1. There's the matter of the different parcellations (268 node, 55 node), which
# means that we couldn't average across those studies, so we would have to 
# separate out the 268 node studies from the 55 node studies and plot those 
# separately probably?
# 2. Then within the 268 node atlas, some of the studies have the full matrix
# instead of just half. So, I need to either 1) figure out how to remove half
# of the matrix (vector -> matrix -> half matrix -> vector), or 2) just separate
# the full matrix from half matrix studies. SIDE NOTE: could having the full 
# matrix studies plotting currently affect the simultaneous confidence intervals?

# average the width of the confidence intervals (e.g. upper bound - point estimate, average those)
# not the exact numbers of each interval. Compare this to just taking the mean
# of the actual confidence interval numbers

# 3. Activation studies would need to be separate from FC studies (sort of 
# related to parcellations)... to even be able to average across activation maps
# I went and removed the part of the effect size code that removed zeros from
# these maps because that made them all have different numbers of voxels. Then
# I'll remove the zeros at the time of plotting I think instead!

# So with those in mind, if I follow my idea of how to implement this, we would
# end up with something like plots of:
# - each stat type (r, t, t2)
#   - each parcellation (268, 268 full matrix, 55)
#   - each map type (FC, activation)
# e.g. r_268_FC, r_268_full_FC, r_55_FC, r_268_act, ..., t2_268_act
# for now, we don't have that many activation maps, and that many t2 studies
# for example, but as we add to it it might get messy.

# So, is there a different way that I should be thinking about this overall to
# avoid having to separate everything out like this?

# Another question: for the confidence intervals, do I just average those too?
# e.g. for all t2 studies, when I average d across studies for each edge, do I
# also just average the upper bounds across studies and lower bounds across studies?
