# How different is the new data from the old data?
library(haven)
library(dplyr)
library(ggplot2)

dat.old = read.delim("HeroViolence.txt")
dat.new = read_spss("HeroViolenceData - FS 14 dataAdam.sav")


setdiff(names(dat.old), names(dat.new))
setdiff(names(dat.new), names(dat.old))

dat.new = dat.new %>% 
  rename(Intercepted = intercepted,
         Location = Location_filter,
         Guilty = Gulty)

dat.old = dat.old %>% 
  mutate(callsyesno = ifelse(Calls > 0, 1, 0))

# Each subject has a unique ID this time, which is good,
# but that makes it difficult to match the old data to the new data.
table(dat.old$Subject)
table(dat.new$Subject)

# The number of rows has shrank, rather than grown, despite the additional data collection
nrow(dat.old)
nrow(dat.new)

# Is it that non-intercepted subjects or missing data were preemptively discarded this time?
dat.old %>% 
  filter(!(is.na(Calls)), Intercepted == 1) %>% 
  nrow()

dat.new %>% 
  filter(!(is.na(Calls)), Intercepted == 1) %>% 
  nrow()

# Where are these subjects going?
table(dat.old$Calls)
table(dat.new$Calls)

dat.old$File = "Old"
dat.new$File = "New"

dat.old.small = select(dat.old, -Date)
dat.new.small = select(dat.new, -Date)

bind_rows(dat.old.small, dat.new.small) %>% 
  ggplot(aes(x = Calls, fill = File)) +
  geom_bar(stat = "count", position = "dodge")
