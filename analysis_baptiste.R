library(tidyverse)
library(lme4)

traitdat <- read_delim("derived_data/DP16_trait_20210922.txt") %>% 
  filter(trait.val > 0)
pmr <- read_csv("derived_data/mass_remain.csv")

######## Making dataset with combination of mass remaining and trait values
pmrt0 <- filter(pmr, time == 0) %>% 
  group_by(code, species, size, codeStem) %>% 
  summarise(time = mean(time), pmr = mean(pmr)) %>% 
  mutate(unique = codeStem)

unique(pmrt0$species)

traitnobdw <- filter(traitdat, !trait %in% c("waterperc", "barkthick", "density") & time == 0)

traitt0 <- left_join(traitnobdw, pmrt0, by = c("unique", "code", "size", "codeStem", "time")) %>% 
  filter(!is.na(trait.val)) %>% 
  ungroup() %>% 
  select(-species) %>% 
  group_by(codeStem, hostSpecies, trait, size, time) %>% 
  summarise(trait.val = mean(trait.val)) %>% 
  mutate(pmr = 1,
         unique = codeStem) 

#missed_sp <- filter(traitnobdw, hostSpecies %in% c("acel", "basp", "cali", "cota", "hase", "isan", "olst", "leer", "ripi", "penu")) %>% 
#  group_by(hostSpecies, size, trait, time) %>% 
#  summarise(trait.val = mean(trait.val))

trait_pmr <- left_join(select(pmr, unique, pmr, time), traitdat, by = c("unique", "time"))

trait_pmr_not0 <- filter(trait_pmr, !is.na(trait))

trait_pmr_fin <- select(trait_pmr_not0, codeStem, hostSpecies, trait, size, time, trait.val, pmr, unique) %>% 
  bind_rows(traitt0)


########## Basic exploratory plots


ggplot(trait_pmr_fin, aes(x=factor(time), y=trait.val, col=size)) + 
  geom_violin() + 
  facet_wrap( ~ trait, ncol=2, scales='free_y')

ggplot(pmr, aes(x = time, y = pmr, color = size)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~species)

filter(trait_pmr_fin, trait == "K") %>% 
  ggplot(aes(x=pmr, y=trait.val, col=hostSpecies)) + 
  geom_point() +
  scale_x_reverse()

ggplot(trait_pmr_fin, aes(x=pmr, y=trait.val)) + 
  geom_point() +
  scale_x_reverse() +
  geom_smooth() +
  facet_wrap(~trait, scales = "free_y")

ggsave("output/traits_vs_massremain.png")

# Calculate initial N and P value

meanNP <- group_by(trait_pmr_fin, codeStem, trait, size) %>% 
  filter(time == 0) %>% 
  summarise(inittrait = mean(trait.val)) %>% 
  filter(trait == "N" | trait == "P") %>% 
  spread(trait, inittrait) %>% 
  mutate(category_n = case_when(
    N <= 0.39 ~ "<0.39",
    N > 0.39 & N <= 0.8 ~  "0.39-0.8",
    N > 0.8 & N <1.1 ~ "1.1"
  ),
  init_n = N,
  init_P = P) %>% 
  select(-c("N", "P"))

# Calculate proportion 

# Species with one original measurement for each stem
propCN <- filter(trait_pmr_fin, !hostSpecies %in% c("acel", "basp", "cali", "cota", "hase", 
                                                   "isan", "olst", "leer", "ripi", "penu")) %>% 
  group_by(codeStem, trait) %>% 
  arrange(codeStem, trait, time) %>% 
  mutate(prop = (trait.val*pmr)/(trait.val[1L]*pmr[1L])) %>%  
  filter(any(time == 0)) %>% 
  ungroup() %>% 
  filter(trait == "C" | trait == "N" | trait == "P" | trait == "Ca" | trait == "Fe" | trait == "K" |
           trait == "Mn" | trait == "K") %>% 
  left_join(meanNP, by = c("codeStem", "size"))

# Species with one original measurement for each species
propCN_final <- filter(trait_pmr_fin, hostSpecies %in% c("acel", "basp", "cali", "cota", "hase", 
                                                    "isan", "olst", "leer", "ripi", "penu")) %>% 
  group_by(hostSpecies, trait) %>% 
  arrange(hostSpecies, trait, time) %>% 
  mutate(prop = (trait.val*pmr)/(trait.val[1L]*pmr[1L])) %>%  
  filter(any(time == 0)) %>% 
  ungroup() %>% 
  filter(trait == "C" | trait == "N" | trait == "P" | trait == "Ca" | trait == "Fe" | trait == "K" |
           trait == "Mn" | trait == "K") %>% 
  bind_rows(propCN)

filter(propCN_final, trait == "P" & size == "large")


# Plot proportions

propCN_final %>% 
  ggplot() +
  scale_x_reverse() +
  #geom_point(aes(x = pmr, y = prop, color = hostSpecies)) +
  geom_smooth(aes(x = pmr, y = prop)) +
  facet_wrap(~trait, scales = "free_y") +
  theme_bw()

ggsave("output/trait_frac_time.png")

# Nitrogen graph against mas remaining
filter(propCN_final, trait == "N") %>% 
  ggplot() +
  geom_point(aes(x = pmr*100, y = prop, color = hostSpecies)) +
  scale_x_reverse() +
  geom_smooth(aes(x = pmr*100, y = prop, color = hostSpecies), se = F) +
  facet_wrap(~size) +
  geom_hline(aes(yintercept = 1)) +
  labs(x = "Mass remaining (%)", y = "Fraction of Initial Nitrogen")

ggsave("output/propN_massremain.png")

# Carbon graph against mass remaining
filter(propCN_final, trait == "K") %>% 
  ggplot() +
  geom_point(aes(x = pmr*100, y = prop, color = hostSpecies)) +
  scale_x_reverse() +
  geom_smooth(aes(x = pmr*100, y = prop)) +
  facet_wrap(~size) +
  labs(x = "Mass remaining (%)", y = "Fraction of Initial Carbon")

ggsave("output/propC_massremain.png")

filter(propCN_final, trait == "K") %>% 
  ggplot(aes(x = trait.val, y = prop, colro = hostSpecies)) +
  geom_point()

# Phosphorus graph against mass remaining
filter(propCN_final, trait == "P" & trait.val > 100) %>% 
  ggplot() +
  geom_point(aes(x = pmr*100, y = prop, color = hostSpecies)) +
  scale_x_reverse() +
  geom_smooth(aes(x = pmr*100, y = prop)) +
  geom_hline(aes(yintercept = 1)) +
  #facet_wrap(~size) +
  labs(x = "Mass remaining (%)", y = "Fraction of Initial Phosphorus")

ggsave("output/propP_remaining.png")


filter(propCN_final, trait == "N") %>% 
  ggplot() +
  geom_point(aes(x = time, y = prop, color = hostSpecies)) +
  geom_smooth(aes(x = time, y = prop)) +
  facet_wrap(~size) +
  labs(x = "Time (Months)", y = "Fraction of Initial Nitrogen")

ggsave("output/propN_time.png")

filter(propCN, trait == "C") %>% 
  ggplot() +
  geom_point(aes(x = time, y = prop, color = hostSpecies)) +
  geom_smooth(aes(x = time, y = prop)) +
  facet_wrap(~size) +
  labs(x = "Time (Months)", y = "Fraction of Initial Carbon")

ggsave("output/propC_time.png")

propCN %>% 
  ggplot() +
  geom_point(aes(x = time, y = pmr, color = size)) +
  geom_smooth(aes(x = time, y = pmr, color = size)) +
  facet_wrap(~hostSpecies) +
  labs(x = "Time (Months)", y = "Mass remaining")

## Apply litterfitter

acel_N <- filter(propCN_final, hostSpecies == "acel" & trait == "N") %>% 
  add_row(pmr = 0, prop = 0)
acpa_N <- filter(propCN_final, hostSpecies == "acpa" & trait == "N" & size == "large") %>% 
  add_row(pmr = 0, prop = 0)
  
ggplot(acpa_N) +
  geom_point(aes(x = pmr*100, y = prop)) +
  scale_x_reverse()

fit<-fit_litter(time=acpa_N$pmr*100,mass.remaining=acpa_N$prop,
                model='import.model',iters=10000)
plot(fit)



# Calculate N:P and N:C ratios

trait_pmr_spread <- spread(trait_pmr_fin, trait, trait.val) %>% 
  rowwise() %>% 
  mutate(CNrat = C/N,
         NPrat = N/P*1000) %>% 
  filter(!is.na(NPrat))

unique(trait_pmr_spread$hostSpecies)

ggplot(trait_pmr_spread, aes(x = pmr, y = CNrat, color = size)) +
  geom_point() +
  scale_x_reverse() +
  geom_smooth()

filter(trait_pmr_spread, P > 100) %>% 
  ggplot(aes(x = pmr, y = NPrat, color = size)) +
  geom_point() +
  scale_x_reverse() +
  geom_smooth()

propCN_final %>%
  select(unique,pmr,hostSpecies, size,trait,trait.val) %>%
  pivot_wider(names_from = trait,values_from=trait.val) %>%
  mutate(NP=N/P*1000) %>%
  filter(P>100) %>%
  ggplot(aes(y=NP,x=pmr,col=size)) +
  #facet_wrap(~hostSpecies) +
  geom_point()+
  scale_x_reverse()+ 
  geom_smooth()+
  ylab("NP ratio")+
  theme_bw()->np_plot

np_plot
ggsave("output/NPratio.png")

## Relate trait in previous step to future decay rate

trait_mass_final <- filter(trait_pmr_fin, !is.na(trait.val))

# Create data with difference 
mass_avg_diff <- group_by(trait_mass_final, time, hostSpecies, size) %>% 
  summarise(pmr_avg = mean(pmr)) %>% 
  arrange(size, hostSpecies, time) %>% 
  ungroup() %>% 
  group_by(hostSpecies, size) %>% 
  mutate(timediff_years = as.numeric(c(0, 0.583, 0.5, 1, 1, 1.83))) %>% 
  summarise(diff_weight = pmr_avg - lag(pmr_avg),
            k_value = -(log(pmr_avg/lag(pmr_avg))/(timediff_years))) %>% 
  mutate(time = as.numeric(c(0, 7, 13, 25, 37, 59)),
         timepred = c("0", "0-7", "7-13", "13-25", "25-37", "37-59")) %>% 
  ungroup()


trait_avg <- group_by(trait_mass_final, time, hostSpecies, size, trait) %>% 
  summarise(trait_avg = mean(trait.val)) %>% 
  arrange(size, hostSpecies, time) %>% 
  ungroup() %>% 
  spread(trait, trait_avg) %>% 
  mutate(timepred = case_when(
    time == 0 ~ "0-7",
    time == 7 ~ "7-13",
    time == 13 ~ "13-25",
    time == 25 ~ "25-37",
    time == 37 ~ "37-59",
    time == 57 ~ "NA"
  ))
  
trait_mass_diff <- left_join(mass_avg_diff, trait_avg, by = c("timepred", "hostSpecies", "size"))

trait_mass_diff$timepred <- factor(trait_mass_diff$timepred, levels = c("0", "0-7", "7-13",
                                                                        "13-25", "25-37", "37-59"))

filter(trait_mass_diff, timepred != 0) %>% 
  ggplot(aes(x = density, y = k_value)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~timepred, scales = "free_x")

ggsave("output/pred_density_time.png")

# Function to convert list to dataframe
list_to_df <- function(mylist){
  
  # make a vector of row ids that correspond to the list names
  rowid.indx <- lapply(mylist, function(x) dim(x)[1])
  sourceVec.list <- list()
  for(i in 1:length(rowid.indx)){
    sourceName <- names(rowid.indx)[i]
    numRows <- rowid.indx[[i]]
    sourceVec.list[[i]] <- rep(sourceName, numRows)
  }
  rowVec <- unlist(sourceVec.list)
  
  # combine into df
  df <- data.frame(do.call(rbind, mylist))
  df$source <- rowVec
  
  return(df)
}

plot_fun_trait <-
  function(data_name) {
    
    data_name <- filter(trait_mass_diff, timepred != 0)  
    
    modout <- lapply(unique(data_name$timepred), function(timepred_val) {
      
      data_name <- filter(data_name, timepred == timepred_val) 
      
      mod <- lmer(k_value ~ C + (1|hostSpecies), data = data_name)
      
      # extract coefficients
      coefs <- data.frame(coef(summary(mod)))
      coefs$names <- row.names(coefs)
      # use normal distribution to approximate p-value
      coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
      coefs
      r2 <- r.squaredGLMM(mod)
    }
           )
    
    names(modout) <- unique(data_name$timepred)
    modout.df <- list_to_df(modout)
    
    
    
    return(modout.df)
  
    }

plot_fun_trait(trait_mass_diff)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)




ggplot(mass_avg_diff, aes(x = time, y = diff_weight, color = size)) +
  geom_point(stat = "summary") +
  geom_errorbar(stat = "summary")

ggplot(mass_avg, aes(x = time, y = pmr_avg, color = size)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~hostSpecies)

