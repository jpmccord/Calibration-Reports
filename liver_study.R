liver_in <- readXMLQQQReport("Liver Study/Connelly Fetal Livers.xml")
control_in <- readXMLQQQReport("Liver Study/Connelly Control Livers.xml")

liver_info <- liver_in %>%
  select(sampleid,name,type,desc,stdconc,calibref,pkflags,area,area1,response1,job,analconc,analconc1) %>%
  rename(Cmp = calibref,
         Sample.Type = type,
         Sample.Name = name,
         Raw.Response = area,
         ISTD.Response = area1,
         Exp.Amt = stdconc,
         Peak.Status = pkflags, # Probably not the right thing to do, working with it for now
         Sample.Desc = desc,
         Batch = job) %>%
  mutate_at(vars(one_of(c("Raw.Response","ISTD.Response","Exp.Amt"))), funs(as.numeric)) %>%
  mutate(Raw.Reponse = ifelse(sampleid %in% c("36","23"), 0, Raw.Response))

control_info <- control_in %>%
  select(sampleid,name,type,desc,stdconc,calibref,pkflags,area,area1,response1,job,analconc,analconc1) %>%
  rename(Cmp = calibref,
         Sample.Type = type,
         Sample.Name = name,
         Raw.Response = area,
         ISTD.Response = area1,
         Exp.Amt = stdconc,
         Peak.Status = pkflags, # Probably not the right thing to do, working with it for now
         Sample.Desc = desc,
         Batch = job) %>%
  mutate_at(vars(one_of(c("Raw.Response","ISTD.Response","Exp.Amt"))), funs(as.numeric))


control_calibration <- control_info %>%
  filter(Sample.Type == "Standard",
         Cmp == "GenX",
         Exp.Amt > 0.2)

control_calibration.model <- lm(data = as.data.frame(control_calibration),
                                response1 ~ Exp.Amt + I(Exp.Amt^2),
                                weights = 1/Exp.Amt)

plotFit(control_calibration.model, interval = "prediction",
        ylab = "Area.Response",
        ylim = c(0,1),
        xlab = "Std.Concentration")

control_samples <- filter(control_info, Sample.Type != "Standard", Cmp =="GenX")

invest(control_calibration.model,
       y0 = as.numeric(control_samples$response1[1]),
       lower = -10,
       upper = 20)


predictions <- lapply(as.numeric(control_samples$response1), invest, lower = -10, upper = 20, object = control_calibration.model, inverval = "Wald")

control_predictions <- do.call(rbind,map(predictions, unlist)) %>% as.data.frame() %>% select(-interval)

control_samples <- cbind(control_samples,control_predictions) %>% 
  mutate(estimate = as.character(estimate),
         estimate = as.numeric(estimate)) %>%
  mutate(lower = as.character(lower),
          lower = as.numeric(lower))%>%
  mutate(upper = as.character(upper),
         upper = as.numeric(upper)) %>%
  mutate(Sample.Desc = gsub("#","", Sample.Desc),
         Sample.Desc = gsub("l","L", Sample.Desc))

liver_calibration <- liver_info %>%
  filter(Sample.Type == "Standard",
         Cmp == "GenX",
         Exp.Amt > 5)

liver_calibration.model <- lm(data = as.data.frame(liver_calibration),
                                Raw.Response ~ Exp.Amt + I(Exp.Amt^2),
                                weights = 1/Exp.Amt)

plotFit(liver_calibration.model, interval = "prediction",
        ylab = "Area.Response",
        xlab = "Std.Concentration")


liver_samples <- filter(liver_info, Sample.Type != "Standard", Cmp =="GenX")

predictions2 <- lapply(as.numeric(liver_samples$response1), invest, lower = -10000, upper = 10000, object = liver_calibration.model, interval = "Wald")

liver_predictions <- do.call(rbind,map(predictions2, unlist)) %>% as.data.frame() %>% select(-interval)

liver_samples <- cbind(liver_samples,liver_predictions) %>% 
  mutate(estimate = as.character(estimate),
         estimate = as.numeric(estimate)) %>%
  mutate(lower = as.character(lower),
         lower = as.numeric(lower))%>%
  mutate(upper = as.character(upper),
         upper = as.numeric(upper)) %>%
  mutate(Sample.Desc = gsub("#","", Sample.Desc),
         Sample.Desc = gsub("l","L", Sample.Desc))

all_sample <- full_join(liver_samples, control_samples) 

mousetype <- c(rep("Maternal",15),rep("Fetal", 60))

sequence <- seq(1,75,1)

litters <- c(rep(seq(1979,1993,1),2)) %>% sort()
sexes_female <- c(rep(c("F1","F2"),15))
sexes_male <- c(rep(c("M1","M2"),15))

IDs_female <- cbind(litter,sexes_female) %>% as.data.frame() %>%
  mutate(ID = paste(litter,sexes_female)) %>%
  rename(sex = sexes_female)

IDs_male <- cbind(litter,sexes_male) %>% as.data.frame() %>%
  mutate(ID = paste(litter,sexes_male)) %>%
  rename(sex = sexes_male)

IDs_maternal <- data.frame(litter = unique(litters)) %>%
  mutate(ID = litter,
         sex = "")

sample_codes <- rbind(IDs_maternal,IDs_female,IDs_male) %>%
  mutate(Sample.No = seq(1:75)) %>%
  mutate(Sample.Desc = paste0("Liver ", Sample.No)) %>%
  mutate(truesex = substr(sex,1,1)) %>%
  mutate(Trt = c(rep(0,3),rep(1,2),rep(3,2),rep(10,2),rep(30,2),rep(62.5,2),rep(125,2),rep(c(rep(0,6),
               rep(1,4),rep(3,4),rep(10,4),rep(30,4),rep(62.5,4),rep(125,4)),2))) %>%
  mutate(Sample.Wt = c(28,32.3,32.3,32.5,41,33.3,37.3,27.4,37.6,43.8,42.2,34.5,36,51.9,
                       31.5,36.9,34.9,38.5,38.9,33.2,38.7,35.8,57.2,36.8,40.2,38.5,42.1,
                       47,35.7,48.3,35.2,39.5,33.5,31.1,31.2,32.4,47.3,35.6,33.3,34.9,
                       46.2,39.8,38.6,51.3,31.5,47.1,44.6,32.4,41.6,64.1,34.2,46.6,37.2,
                       39.4,48.1,34.3,48.6,33.7,37.4,44.3,42,40,40.8,47,40.4,66,40.2,41.9,
                       54.9,45.4,38.4,46.8,30.9,58.5,49.5),
         Water.Added = c(rep(90,4),120,90,120,90,120,120,120,90,90,150,90,rep(100,7),
                         150,100,120,100,120,150,100,150,100,120,rep(100,4),150,100,
                         100,100,150,120,120,150,100,120,120,100,120,150,100,120,100,120,
                         150,100,150,100,100,rep(120,4),150,120,150,120,120,150,150,120,120,100,150,150),
         dil.factor = Sample.Wt/Water.Added,
         Type = ifelse(truesex == "", "Maternal", "Fetal"))

data_display <- full_join(sample_codes, all_sample) %>%
  mutate(adj.conc = estimate/dil.factor) %>%
  filter(!Sample.Desc %in% (c("DB End"))) %>%
  mutate(mousetype = ifelse(truesex == "", "maternal", "fetal"))

stripchart(filter(data_display, is.na(adj.conc))$estimate, xlim = c(-5,20))

boxplot(data = filter(data_display, truesex != ""), adj.conc ~ truesex + Trt)

comparison <- lm(data = filter(data_display, truesex != ""), adj.conc ~ sex + Trt)

car::Anova(comparison)

comparison3 <- lmer(data = filter(data_display, truesex != ""),
                    adj.conc ~ truesex + Trt + (1|litter))

tidy(comparison3)

summary(comparison3)

car::Anova(comparison3)


lattice::xyplot(data = filter(data_display, truesex != ""),
       adj.conc ~ Trt | litter)

qqnorm(log10(data_display$adj.conc))

ggplot(filter(data_display, !is.na(truesex))) +
  theme_bw()+
  geom_point(aes(x = mousetype, y=adj.conc, color=truesex)) +
  facet_wrap(~Trt, scales = "free_y")

ggplot(filter(data_display, !is.na(truesex)))+
  theme_bw()+
  geom_boxplot(aes(y = adj.conc, color = truesex)) +
  facet_wrap(~Trt, scales = "free_y")

ggplot(filter(data_display, !is.na(truesex)))+
  theme_bw()+
  geom_point(aes(x = Trt, y = adj.conc, color = truesex))

ggplot(filter(data_display, !is.na(truesex)))+
  theme_bw()+
  geom_point(aes(x = Trt, y = adj.conc, color = truesex)) +
  facet_wrap(~litter)


