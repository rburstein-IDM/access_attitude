

# mod1 simple (no covariates)
cmod1 <- glm(zd ~ WTT_cut, family = binomial, data = df)
summ(cmod1, exp=T) 

# mod 2. add the intent question
cmod2 <- glm(zd ~ WTT_cut + attitude_simple, family = binomial, data = df)
summ(cmod2, exp=T) 

# mod 3. add controls
cmod3 <- glm(zd ~ WTT_cut + attitude_simple 
               + m_age_level + hh_edu_level + n_u5_level
               + source,
               family = binomial, data = df)
summ(cm0d3, exp=T) 

# Check for distance/intent interaction
cmod4 <- glm(zd ~ WTT_cut * attitude_simple 
               + m_age_level + hh_edu_level + n_u5_level
               + source,
               family = binomial, data = df)
summ(cmod4, exp=T) 


# output the models 
summ(cmod1, exp=T); summ(cmod2.1, exp=T); summ(cmod2.2, exp=T);summ(cmod2.3, exp=T)


