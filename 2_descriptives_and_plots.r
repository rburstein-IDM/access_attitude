

## -------------------------------------
## TRACK WHICH DATA ARE KEPT FOR ANALYSIS, DROPPED AT EACH POINT

dfOrig <- copy(df)

message(paste0('Start with ', nrow(df),' observations.'))

# Keep Rural HHs only
df <- df[rural==1]
message(paste0('Drop urban HHs, now have ', nrow(df),' observations.'))

# Keep children 12-23mo
df <- df[c_agem >=12]
message(paste0('Drop under-1s, now have ', nrow(df),' observations.'))
table(df$source)

# Drop any with missing GPS
df <- df[DQFLAG_miss_gps==FALSE]
message(paste0('Drop missing GPS, now have ', nrow(df),' observations.'))

# Drop duplicated GPS
df <- df[DQFLAG_dup_gps==FALSE]
message(paste0('Drop duplicated GPS, now have ', nrow(df),' observations.'))

# Drop any remaining with missing
df <- df[DQFLAG_miss_tt==FALSE]
message(paste0('Drop any missing Travel Time, now have ', nrow(df),' observations.'))


# how many obs by source year
table(df$source)

# Drop any with missing demographic variables (HAPPENS IN REGRESSION STEP)

# simple coverage 
table(df$vxstatus)
table(df$vxstatus)/nrow(df)

# weighted coverage
weighted.mean(as.numeric(df$vxstatus=='Zero-Dose'),df$ponderation)


## geog accessibility in sample
mean(df$wtt)
df[,.(wtt=mean(wtt)),by=source]
median(df$wtt)
df[,.(wtt=median(wtt)),by=source]
quantile(df$wtt)

tmp <- df[order(wtt)]
tmp[,pond:=1]
tmp[, cumsumm := cumsum(pond)]
tmp[,p:=cumsumm/max(cumsumm)]

tmp[wtt==60] # 88%
tmp[wtt==120] # 95%

df[,.(zd=mean(vxstatus=='Zero-Dose')),by=WTT_cut][order(WTT_cut)]



## ------------------------------------ 
## Plots

# (NOT USED IN MANUSCRIPT)
# overall effect of distance plotted -- accessmod walking versus motor is very different
# interesting difference 
ggplot(df) + 
  geom_smooth(aes(wtt,zd*100),  
              formula = y ~ splines::bs(x, k = 20),
              fill = '#937DC2', color = '#937DC2') +
    scale_x_continuous(limits=c(0,120), breaks = seq(0,120,by=10),
                       name='Minutes to nearest health center (Walking)') +
    scale_y_continuous(
      name='Percent (%) of rural children that are Zero-Dose') +
    theme(panel.grid.major = element_line(color='grey'))



## FIGURE 2a
ggplot(df) +
    geom_smooth(aes(wtt,zd*100,fill = source, color = source),  
               formula = y ~ splines::bs(x, k = 20)) +
    scale_x_continuous(limits=c(0,120), breaks = seq(0,120,by=10),
                       name='Minutes to nearest health facility (Walking)') +
    scale_y_continuous(
      name='Percent (%) of rural children that are Zero-Dose') +
    scale_fill_manual(values=c(clz2[c(1,6)]),name='Survey Round') +
    scale_colour_manual(values=c(clz2[c(1,6)]),name='Survey Round') +
    theme(panel.grid.major = element_line(color='grey'),
          legend.position = 'top')


# FIGURE 2b
tmp <- df[order(attitude_simple,wtt)]
tmp[, cumsumm := cumsum(ponderation),by=attitude_simple]
tmp[,p:=cumsumm/max(cumsumm),by=attitude_simple]

ggplot(df) +
  geom_smooth(aes(wtt,zd*100, 
                  fill=attitude_simple   , color=attitude_simple   ),
              method = "gam", formula = y ~ s(x, k = 5)) +
  scale_fill_manual(values=c('lightgrey','#395B64','#2C3333'),
                    name='How good do you think vaccines are for your child?') +
  scale_colour_manual(values=c('lightgrey','#395B64','#2C3333'),
                      name='How good do you think vaccines are for your child?') +
  scale_x_continuous(limits=c(0,120), breaks = seq(0,120,by=10),
                     name='Minutes to nearest health facility (Walking)') +
  scale_y_continuous(limits=c(0,110), breaks = seq(0,100,by=25),
    name='Percent (%) of rural children that are Zero-Dose') +
  guides(colour = guide_legend(ncol = 1), fill = guide_legend(ncol = 1)) +
  theme(panel.grid.major = element_line(color='grey'),
        legend.position='top')


## (NOT USED IN MANUSCRIPT -- overall attitude by suvey round)
tmp <- df[,.(n=.N),by=.(attitude_simple,source)]
tmp[,pct:=n/sum(n),by=source]
ggplot(tmp) +
  geom_bar(aes(x=source,y=pct*100,fill=attitude_simple),stat='identity',position='stack')+
  scale_fill_manual(values=c('lightgrey','#395B64','#2C3333'),name='') + #,'#A5C9CA'
  ylab('Percent of the population in intent category') +
  xlab('Survey')
  

## SUPP FIGURE 1
ggplot(df) +
  geom_smooth(aes(as.numeric(wtt)/60,as.numeric(df$vs89_1==1)),
              color='#395B64',fill='#395B64') +
  scale_x_continuous(limits=c(0,5), breaks = seq(0,5,by=1),
                     name='Hours to nearest health facility (Walking)') +
  scale_y_continuous(label=scales::percent,
    name='Percent (%) of caregivers who said distance is a reason for non-vaccination') 


# Make Table 1
t1 <- table1(~ WTT_cut + attitude_simple + m_age_level + hh_edu_level + n_u5_level | vxstatus + source, data=df)


