

## Prep ECV2021, ECV2022


## LOAD DATA -------------------------------------------------------

## ECV 2021
if(Sys.info()[['user']]=='royb') {
  df.ecv2021    <- readRDS(file.path(paths$dataloc,'/ECV 2022/dataset/ECV_2021_Enfants.RDS'))
} else {
  df.ecv2021 <- import("data/ecv/2021/ECV_2021_Enfants.rds")
}

## ECV 2022
if(Sys.info()[['user']]=='royb') {
  df.ecv2022 <- readRDS(file.path(paths$dataloc,'/ECV 2023/ECV_2022_Enfants.RDS'))
} else {
  df.ecv2022 <- import("data/ecv/2022/ECV_2022.rds")
}

# load in accessmod map (constructed by WHO)
mtt <- rast(file.path(paths$data,'./WHO_ACCESSMOD/Motorized Travel Time.img'))
wtt <- rast(file.path(paths$data,'./WHO_ACCESSMOD/Walking Travel Time.img'))

## Subset and rename
cb <- fread("./codebook_ecv.csv", header = T)
cb <- cb[names!=""]

# remove village ID 
cb <- cb[var2021!='q106']

# set names according to codebooks
df.ecv2021 <- df.ecv2021[,cb[var2021!='']$var2021,with=F]
setnames(df.ecv2021,cb[var2021!=""]$var2021, cb[var2021!=""]$names)
df.ecv2022 <- df.ecv2022[,cb[var2022!='']$var2022,with=F]
setnames(df.ecv2022,cb[var2022!=""]$var2022, cb[var2022!=""]$names)

## Align var types for date vars
date_vars <- grep("date_", names(df.ecv2022), value=T)
for (var in date_vars) df.ecv2022[[var]] <- ymd(df.ecv2022[[var]])

## Append both years of data to make one data set
df <- rbind(df.ecv2021 %>% mutate(source="ecv2021"), df.ecv2022 %>% mutate(source="ecv2022"), fill=T, use.names=T)

# stable child id variable for later
df[,id:=1:.N]


## VACCINATION COVERAGE --------------------------------------------------------

## Binarize vaccination status
cols <- names(df) %>% str_subset("vacc_|knowledge_")
df <- df %>% mutate_at(cols, function(x) ifelse(x==2, 0, x))

## Zero-dose (Gavi definition of penta1==0)
df[, zd := case_when(vacc_penta1==0~1,
                     vacc_penta1==1~0)]

## Under-immunized (MCV1==0 & penta1==1)
df[, ui := as.numeric(vacc_penta1==1&(vacc_mcv1==0|vacc_penta3==0))]

## Vaccination status variable
df[, vxstatus := 'Complete']
df[ui==1, vxstatus := 'Under-Immunized']
df[zd==1, vxstatus := 'Zero-Dose']



## DEMOG. VARS --------------------------------------------------------

## Month of birth variable - for merging with Supervision
df[, month := ymd(paste0(c_dob_y, "-", c_dob_m, "-", 1))]

## Urbanicity
df[, rural := case_when(rural==1 ~ 0,
                        rural==2 ~ 1)]

## Advanced sessions
df[, ecv_session_mobile := case_when(fr_mobile%in%c("1", "Oui") ~ "yes advanced",
                                     fr_mobile%in%c("2", "Non")~ "no")]

## Month of vaccinations
df[, month_penta1 := ymd(paste0(year(date_penta1), "-", month(date_penta1), "-", 1))]
df[, month_penta1 := ymd(paste0(year(date_penta3), "-", month(date_penta3), "-", 1))]

## Demographics

## Maternal education
df[, m_edu := case_when(m_edu%in%c("0")|grepl("jamais", m_edu)~"Less than primary", 
                        m_edu%in%c("1", "Primaire")~"Primary", 
                        m_edu%in%c("2", "Secondiare")~"Secondary",
                        m_edu%in%c("3", "Supérieur")~"Tertiary")]

df[, hh_edu_level := case_when(hh_edu%in%c("0")|grepl("jamais", hh_edu)~"Less than primary", 
                         hh_edu%in%c("1", "Primaire")~"Primary", 
                         hh_edu%in%c("2", "Secondiare")~"Secondary",
                         hh_edu%in%c("3", "Supérieur")~"Tertiary")]
df[,hh_edu_level := factor(hh_edu_level,levels=c('Tertiary', 'Secondary', 'Primary', 'Less than primary'))]                               

## Is mom employed
df[, m_emp := ifelse(m_prof>0, 1, 0)]

## Maternal religion
df[, m_religion := case_when(m_religion%in%c("2", "Catholique")~"Catholic", 
                             m_religion%in%c("3", "Protestante")~"Protestant", 
                             m_religion%in%c("4", "Kimbanguiste")~"Kimbanguist",
                             m_religion%in%c("5", "Musulmane")~"Muslim",
                             m_religion>=6|m_religion==1|grepl("Autre|Pas",m_religion)~"Other") %>% as.factor]


## HH Head Ethnicity
df[, hh_ethnicity := case_when(hh_ethnicity%in%c("1", "Luba")~"Luba",
                               hh_ethnicity%in%c("2", "Kongo")~"Kongo",
                               hh_ethnicity%in%c("3", "Swahili")~"Swahili",
                               hh_ethnicity%in%c("4", "Mungala")~"Mungala")]

df[, n_u5_level := case_when(n_u5==1 ~ "1", 
                             n_u5==2 ~ "2",
                             n_u5>=3 ~ "3+")]


# moms age
df[, m_age_level := case_when(m_age < 18 ~ "<18", 
                              m_age >= 18 & m_age < 20 ~ "18-19",
                              m_age >= 20 & m_age < 25 ~ "20-24",
                              m_age >= 25 & m_age < 30 ~ "25-29",
                              m_age >= 30 & m_age < 40 ~ "30-39",
                              m_age >40 ~ ">40" )]
df[,m_age_level  := factor(m_age_level ,levels=c('<18', '18-19', '20-24', '25-29', '30-39','>40'))] 


# kids age category
df[, child_age := ifelse(c_agem < 12, '6-12mo','12-23mo')]
df[, child_age  := factor(child_age ,levels=c('6-12mo','12-23mo'))] 

## ACCESS - EXTRACT DISTANCE FROM ACCESSMOD MAP AT EACH ECV COORD -------------

# extract distances from mtt raster at gps points
ecvcoords <- na.omit(df[, c("longitude", "latitude","id")])
coordinates(ecvcoords) <- ~longitude+latitude
proj4string(ecvcoords) <- CRS("+init=epsg:4326") # WGS84
ecvcoords <- spTransform(ecvcoords, crs(mtt))


# extract the tt values
ecvmtt <- extract(mtt, 
                  vect(ecvcoords,  geom=c("longitude", "latitude"), 
                       crs=crs(mtt))) %>% 
  data.table() %>%   cbind(id=ecvcoords$id)
ecvmtt <- ecvmtt[,2:3]
setnames(ecvmtt,'Layer_1','mtt')

ecvwtt <- extract(wtt, 
                  vect(ecvcoords,  geom=c("longitude", "latitude"), 
                       crs=crs(wtt))) %>% 
  data.table() %>%   cbind(id=ecvcoords$id)
ecvwtt <- ecvwtt[,2:3]
setnames(ecvwtt,'Layer_1','wtt')

df <- merge(df,ecvmtt,by='id',all.x=TRUE)
df <- merge(df,ecvwtt,by='id',all.x=TRUE)

# Make cut variables for regression
hist(df$wtt[df$wtt<120])

# or just use: 0 - 0.5, 0.5 - 1.0, 1.0+ for simplicity
labs <- c('<5 min','5-10 min', '10-20 min', '20-40min', '40-60min','60+ min')
df[, WTT_cut :=  cut(wtt, breaks = c(-1,5,10,20,40,60,100000), labels = labs)]
df[, MTT_cut :=  cut(mtt, breaks = c(-1,5,10,20,40,60,100000), labels = labs)]
df[,.(zd=mean(zd)),by=WTT_cut]; df[,.(zd=mean(zd)),by=MTT_cut]; 
table(df[is.na(WTT_cut)]$wtt)


## INTENT - USE ONE QUESTION TO RULE THEM ALL --------------------------------

# prep the intent question we will use
table(df$attitude_good,df$source)
df[,.(m=mean(vxstatus=='Zero-Dose')),by=attitude_good]
# QA601 How good do you think vaccines are for your child?
  # 1 Very good
  # 2 Good
  # 3 Bad
  # 4 Very bad
  # 5 Do not know
# re-categorize 1,2,3-5 
df[attitude_good%in%c(3:5), attitude_simple  := "Bad, Very Bad, Don't Know"]
df[attitude_good%in%c(2),   attitude_simple  := "Good"]
df[attitude_good%in%c(1),   attitude_simple  := "Very Good"]
df[,attitude_simple := factor(attitude_simple, levels=c('Very Good','Good',"Bad, Very Bad, Don't Know"))]

df[,.(ZD_PREV=mean(vxstatus=='Zero-Dose'),n=.N,tt=mean(wtt,na.rm=T)),by=attitude_simple][order(attitude_simple)]



## FLAG DATA ISSUES ---------------------------------------------------------

# flag duplicates (note, later use Kerry's flags for this)
df[, DQFLAG_dup_gps := duplicated(latitude,longitude)] 
(df[,.(avg_dups=mean(DQFLAG_dup_gps)),by=source]) # more in ECV1, 
# flag missing gps
df[, DQFLAG_miss_gps := is.na(latitude) | is.na(longitude)] 
(df[,.(miss_gps=mean(DQFLAG_miss_gps),N=sum(DQFLAG_miss_gps)),by=source]) # more in ECV1, 
# flag missing accessmod
df[, DQFLAG_miss_tt := is.na(wtt) | is.na(mtt)] 
(df[,.(miss_gps=mean(DQFLAG_miss_tt),N=sum(DQFLAG_miss_tt)),by=source]) # more in ECV1, 




