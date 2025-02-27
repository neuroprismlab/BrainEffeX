# v starting at 1 GB

# first, downsample spatial brain masks to keep only every 5th slice - how?
# put NA in the other slices? 

# check size of one act map:
object.size(v$data$hcp_act_t_emotion$pooling.none.motion.none.mv.none$d)

downsample = 100



# show the size of each part of the data
for (study in names(v$data)) {
  size = object.size(v$data[[study]])/1000000
  print(paste0('study: ', study, '. size: ', size))
}

# hcp activation studies are about 44mb each, whereas others are around 10mb

# what about within each study? which parts of the data are largest?
size <- list()
for (study in names(v$data)) {
  for (combo in names(v$data[[study]])) {
    for (var in names(v$data[[study]][[combo]])) {
      s = object.size(v$data[[study]][[combo]][[var]])/1000000
      size[[study]][[combo]][[var]] <- s
    }
  }
}

# should get rid of p, stat, etc. things that we don't need for the app
# that whole data file can be downloaded elsewhere


# move the following to a new function of combine_gl

# remove stat, p, std.brain, std.score if they exist
for (study in names(data)) {
  for (combo in names(v$data[[study]])) {
    if ('stat' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$stat <- NULL
    }
    if ('p' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$p <- NULL
    }
    if ('std.brain' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$std.brain <- NULL
    }
    if ('std.score' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$std.score <- NULL
    }
    if ('p.fullres' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$p.fullres <- NULL
    }
    if ('stat.fullres' %in% names(data[[study]][[combo]])) {
      data[[study]][[combo]]$stat.fullres <- NULL
    }
  }
}

# new size after that: 500mb

# then if we make the 12 different data files to access with API...
# use compress_and_separate.R file!
# now the largest combo file is the regression univariate one at 200mb - not bad!

# TODO: ADD TYPE OF MOTION REGRESSION BACK INTO THE APP!!!
