
fit_model <- function(rec,mod) {
  fit(mod,LeagueIndex~.,data=bake(rec,NULL,all_predictors(),all_outcomes()))
}



#function to make predictions using tree
pred_model <- function(spl,rec,mod) {
  mod_baked <- bake(rec,new_data=assessment(spl),all_predictors(),all_outcomes())
  out <- mod_baked %>% select(LeagueIndex)
  if (is.numeric(out$LeagueIndex)) {
    predicted <- predict(mod,mod_baked)
  }
  else{
  predicted <- predict(mod,mod_baked,type="prob")
  }
  out <- out %>% cbind(predicted)
  names(out)[1] <- "truth"
  out
}


#function to apply recipe to splits
train_recipe <- function(rec,rec_name,spl,fit_mod) {
  spl_prep <- map(spl,prepper,recipe=rec)
  spl_fit <- map(spl_prep,.f=fit_model,mod=fit_mod)
  spl_pred <- pmap(lst(spl=spl,rec=spl_prep,mod=spl_fit),pred_model)
  #out <- map(spl_pred,OnevRest,truth)
  out <-c()
  for (i in 1:length(spl_pred)) {
    current_split <- spl_pred[[i]]
    if (is.numeric(current_split$truth)) {
      current_split <- current_split %>% mutate(prediction = cutoff(.pred,0.354))
      new_val <- OnevRest(current_split,"truth","prediction")
    }
    else {
      new_val <- roc_auc(current_split,"truth",starts_with(".pred")) %>% pull(.estimate)
    }
    out <- c(out,new_val)
  }
  out
}

#function to calculate values for each split
calculate_splits <- function(x,lst_recs,fit_mod) {
  temp_split <- x %>% select(id)
  for(i in 1:length(lst_recs)) {
    #print(i)
    #print(names(lst_recs)[i])
    y <- train_recipe(lst_recs[[i]],names(lst_recs)[i],cv_splits$splits,fit_mod )
    nm <- names(lst_recs)[i]
    temp_split <- temp_split %>% mutate(!!nm := y)
  }
  temp_split
}

# calculate splits function, with separation due to missingness
calculate_splits_sep <- function(x,lst_recs,fit_mod,x_sep) {
  temp_split <- x %>% select(id)
  for(i in 1:length(lst_recs)) {
    #print(i)
    #print(names(lst_recs)[i])
    y <- train_recipe_sep(lst_recs[[i]],names(lst_recs)[i],cv_splits_res$splits,fit_mod,x_sep )
    nm <- names(lst_recs)[i]
    temp_split <- temp_split %>% mutate(!!nm := y)
  }
  temp_split
}

#train recipe function with separation due to missing data
train_recipe_sep <- function(rec,rec_name,spl,fit_mod,x_sep) {
  spl_prep <- map(spl,prepper,recipe=rec)
  spl_fit <- map(spl_prep,.f=fit_model,mod=fit_mod)
  spl_pred <- pmap(lst(spl=spl,rec=spl_prep,mod=spl_fit),pred_model)
  #out <- map(spl_pred,OnevRest,truth)
  out <-c()
  for (i in 1:length(spl_pred)) {
    current_sep <- x_sep %>% filter(cv_split==i) %>% mutate(prediction = 8) %>% rename(truth = LeagueIndex) %>% select(truth,prediction) 
    current_split <- spl_pred[[i]]
    current_split <- current_split %>% mutate(prediction = cutoff(.pred,0.354)) %>% select(-`.pred`)
    current_split <- current_split %>% rbind(current_sep)
    new_val <- OnevRest(current_split,"truth","prediction")
    out <- c(out,new_val)
  }
  out
}


# calculate One Vs Rest AUC
#OvR - single out one class and check how well the model singles it out compared to all other options
OnevRest <- function(db,truth,guess) {
  one <- db %>% pull(truth) %>% unique(.) %>% sort(.)
  auc_vec <- numeric(0)
  for (i in one) {
    trial_set <- db %>% select(c(!!truth,!!guess)) %>% mutate(across(c(!!truth,!!guess),~ifelse(.x==i,1,0)))
    truth_fac <- factor(trial_set%>%pull(truth),levels=c(1,0))
    trial_set <- trial_set %>% cbind(truth_fac)
    new_val <- roc_auc(trial_set,truth_fac,guess)$.estimate
    if (new_val<0.5) { new_val<- 1- new_val }
    auc_vec <- c(auc_vec,new_val)
  }
  mean(auc_vec)
}




#function to get significance of second power effect, used to check linearity
Linearity_test <- function(dat,x,y) {
  dat <- dat %>% mutate(x_sq = get(x)^2)
  mod_current <- lm(dat%>%pull(y)~x_sq+dat%>%pull(x),data = dat)
  res <- mod_current$coefficients[2]
  names(res) <- x
  res
}

cutoff <- function(vec,x) {
  res <- floor(vec)
  res[vec-res>=x] <- res[vec-res>=x]+1
  res
}
