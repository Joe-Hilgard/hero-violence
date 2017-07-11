# How different is the new data from the old data?
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)

dat.old = read.delim("./partial_datafiles/HeroViolence.txt")
dat.old2 = read.csv("./partial_datafiles/HeroViolenceData - final complete data file.csv", stringsAsFactors = F)
dat.new = read_spss("./partial_datafiles/HeroViolenceData - FS 14 dataAdam.sav")


setdiff(names(dat.old), names(dat.new))
setdiff(names(dat.new), names(dat.old))

# Coerce column names to match
dat.new = dat.new %>% 
  rename(Intercepted = intercepted,
         Location = Location_filter,
         Guilty = Gulty)

dat.old = dat.old %>% 
  mutate(callsyesno = ifelse(Calls > 0, 1, 0)) %>% 
  rename(Guilty = Gulty)

# Each subject has a unique ID this time, which is good,
# but that makes it difficult to match the old data to the new data.
table(dat.old$Subject)
table(dat.new$Subject)

# The number of rows has shrank, rather than grown, despite the additional data collection
nrow(dat.old)
nrow(dat.new)

# Is it that non-intercepted subjects or missing data were preemptively discarded this time?
# apparently not. So why?
dat.old %>% 
  filter(!(is.na(Calls)), Intercepted == 1) %>% 
  nrow() # old: 191 valid observations

dat.new %>% 
  filter(!(is.na(Calls)), Intercepted == 1) %>% 
  nrow() # new: 189 valid observations

# are they indeed identical for rows 1-185?
dat1 <- dat.new[1:185, ]
dat2 <- dat.old[1:185, ]
names(dat2) <- paste0(names(dat2), ".old")
cbind(dat1, dat2) %>% 
  ggplot(aes(x = Calls, y = Calls.old)) +
  geom_point()

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

# 31 CMU participants were excluded, 19 added?
dat.old <- mutate(dat.old, lubDate = mdy(Date)) # 2 NAs failed to parse
dat.new <- mutate(dat.new, lubDate = as_date(Date))

# Exclusion of 30 participants
filter(dat.old, 
       Location == 1, # CMU
       lubDate %within% (mdy("9/30/2014") %--% mdy("10/16/2014"))) %>% 
       nrow()

# Addition of 19 new subjects
filter(dat.new,
       Location == 1, 
       lubDate %within% (mdy("8/1/2015") %--% mdy("5/1/2016"))) %>% 
  nrow()

filter(dat.old, !is.na(Location)) %>% 
  ggplot(aes(x = lubDate, y = Calls)) +
  geom_point() +
  facet_wrap(~Location) +
  scale_x_date(limits = c(min(dat.old$lubDate, na.rm = T), max(dat.new$lubDate, na.rm = T)))

dat.new %>% 
  ggplot(aes(x = lubDate, y = Calls)) +
  geom_point() +
  facet_wrap(~Location) +
  scale_x_date(limits = c(min(dat.old$lubDate, na.rm = T), max(dat.new$lubDate, na.rm = T)))

# accounting

