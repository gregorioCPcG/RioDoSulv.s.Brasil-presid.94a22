library(ggplot2)

ano <- c("2022-10-1","2018-10-1",
         "2014-10-1","2010-10-1","2006-10-1",
         "2002-10-1","1998-10-1","1994-10-1")
ano <- as.data.frame.POSIXlt(ano)
name <- c("Lula2022","Haddad2018","Dilma2014","Dilma2010","Lula2006","Lula2002",
          "Lula1998","Lula1994")
voto <- c(22.64,10.08,24.76,35.55,29.24,44.11,27.32,18.79)
df <- data.frame(ano,voto)
a <- ggplot(df, aes(ano,voto))
a<- a +  geom_line(stat = "summary", color="red")+ 
  labs(title = "Candidato de esquerda mais votado na cidade",
       subtitle = "Rio do Sul (SC) - presidencial primeiro turno 1994-2022",
       x = "Data", y= "% votos válidos", caption = "fonte: TRE-SC")+ 
  geom_label(aes(label = name), nudge_x = 0.01,nudge_y = 0.011,
             check_overlap = TRUE)
a
library(gridExtra)
voto <- c(48.43,29.28,41.59,46.91,48.61,46.44,31.71,27.07)
df <- data.frame(ano,voto)
b <- ggplot(df, aes(ano,voto))
b<- b +  geom_line(stat = "summary", color="red")+ 
  labs(title = "Candidato de esquerda mais votado ",
       subtitle = "Brasil - presidencial primeiro turno 1994-2022",
       x = "Data", y= "% votos válidos", caption = "fonte: TSE")+ 
  geom_label(aes(label = name), nudge_x = 0.01,nudge_y = 0.011,
             check_overlap = TRUE)
b
grid.arrange(a,b,nrow=2)

# 2002,2006,2010
ano <- c("2010-10-1","2010-10-1",
         "2006-10-1","2006-10-1",
         "2002-10-1","2002-10-1")
name <- c("Dilma10_rsl","Dilma10_br","Lula06_rsl","Lula06_br","Lula02_rsl","Lula02_br")
ano <- as.data.frame.POSIXlt(ano)
voto <- c(35.55,46.91,29.24,48.61,44.11,46.44)

df <- data.frame(ano,voto)
c <- ggplot(df, aes(ano,voto))
c<- c +  geom_line(stat = "summary", color="red")+ 
  labs(title = "Candidato de esquerda mais votado ",
       subtitle = "Rio do Sul x Brasil - presidencial primeiro turno 2002-2010",
       x = "Data", y= "% votos válidos", caption = "br = Brasil, rsl = RioDoSul")+ 
  geom_label(aes(label = name), nudge_x = 0.01,nudge_y = 0.011,
             check_overlap = TRUE)
c


# 2018 e 2022

partido <- c("PT","Bolsonaro","Outros Candidatos","PT","Bolsonaro","Outros Candidatos")
ano <- c(2018,2018,2018,2022,2022,2022)
votos <- c(10.08,73.57,16.35,22.64,69.89,7.47)
final <- data.frame(partido,ano,votos)
d <- ggplot(final, aes(x=as.character(ano), y=votos, fill=partido)) + geom_col(position = "fill") + geom_hline(yintercept = .5, alpha=.75, linetype="dashed") +
geom_hline(yintercept = .25, linetype="dotted", size=.20) +
geom_hline(yintercept = .75, linetype="dotted", size=.25) + scale_y_continuous(labels=scales::percent) +
  labs(title= "RioDoSul-SC primeiro turno",y="Percentual", x="Eleição", caption="% votos váliddos")
                                                  + scale_fill_manual(values=c('black', '#1b1464',
                             '#2e3192',
                             '#0071bc',
                             '#999999',
                             '#39b54a',
                             '#93278f',
                             '#9e005d',
                             '#c1272d',
                             '#ffff00'), drop=F, "partido/cand.") +theme(text = element_text(family="Bahnschrift", size=12),
        plot.title = element_text(size=24)) +  theme(panel.spacing = unit(1.15, "lines"))
d

votos <- c(29.28,46.03,24.69,48.43,43.20,8.37)
final <- data.frame(partido,ano,votos)
e <- ggplot(final, aes(x=as.character(ano), y=votos, fill=partido)) + geom_col(position = "fill") + geom_hline(yintercept = .5, alpha=.75, linetype="dashed") +
  geom_hline(yintercept = .25, linetype="dotted", size=.20) +
  geom_hline(yintercept = .75, linetype="dotted", size=.25) + scale_y_continuous(labels=scales::percent) +
  labs(title= "Brasil primeiro turno",y="Percentual", x="Eleição", caption="% votos váliddos")
+ scale_fill_manual(values=c('black', '#1b1464',
                             '#2e3192',
                             '#0071bc',
                             '#999999',
                             '#39b54a',
                             '#93278f',
                             '#9e005d',
                             '#c1272d',
                             '#ffff00'), drop=F, "partido/cand.") +theme(text = element_text(family="Bahnschrift", size=12),
                                                                         plot.title = element_text(size=24)) +  theme(panel.spacing = unit(1.15, "lines"))
e
grid.arrange(d,e,ncol=2)

#
voto_RSL <- c(22.64,10.08,24.76,35.55,29.24,44.11,27.32,18.79)
voto <- c(48.43,29.28,41.59,46.91,48.61,46.44,31.71,27.07)
ra <- voto - voto_RSL
ano <- c("2022-10-1","2018-10-1",
         "2014-10-1","2010-10-1","2006-10-1",
         "2002-10-1","1998-10-1","1994-10-1")
ano <- as.data.frame.POSIXlt(ano)
lin1 <- data.frame(ra,ano)
lin <- ggplot(lin1, aes(ano, ra))
lin + geom_line(aes(colour = "black"), size = 1.5) + 
  geom_point(size = 3)+ 
  labs(title="Votação no PT no Brasil menos Votação no PT em Rio do Sul",
       subtitle="Primeiros turnos 1994-2022", x = "Data", y = "Diferença pró Brasil",
       caption = "Fonte: TSE")
+ theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom")
+ guides(linetype = F, shape = F)