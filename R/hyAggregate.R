rl_sciencebase <-
  'https://www.sciencebase.gov/catalog/file/get/60c92503d34e86b9389df1c9?name=enhd_nhdplusatts.fst'

hyAggregate_env <- new.env()

assign("ngen_dat_dir",
       tools::R_user_dir("hyAggregate"),
       envir = hyAggregate_env)
