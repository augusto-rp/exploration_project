library(psych)
library(misty)
library(naniar)
library(mice)
library(mvnmle)


library(mclust)
library(robustbase)
library(tidyLPA)

#tribunales, gobierno, congreso, municipalidades, fiscales
#"confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p"

#primero abrir archivo que esta en subcarpeta bases_sinmodificacion llamado "base_92_20112024.csv y crear objeto llamado base
base<-read.csv("bases_sinmodificacion/base_92_20112024.csv", header=T)




#crear base de datos llamada "df" extrayendo columnas "id_bu", "gse", "percepcion_2", "percepcion_3", "bienestar_2", "percepcion_5", "percepcion_6", "percepcion_38, "democracia_21", "sexo", "edad" )
df <- base[, c("id_bu", "gse", "percepcion_2", "percepcion_3", "percepcion_4", "bienestar_2", "percepcion_5", "percepcion_6", "percepcion_38", "democracia_21","sexo", "edad","confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p")]
#luego eliminar filas con NA en cualquier columna


#si valor es -8 o -9 remplazar por NA
df[df == -8 | df == -9 | df ==88 | df==99] <- NA



#veamos si datos perdidos son aleatorios
#misty::na.test(df) #es estadisticamente significativa por lo que patron de datos eprdidos no es aleatorio, pdoria haber hecho imputacion pero no se eprdio tanto

df <- na.omit(df) #sacar datos perdidos

describe(df)


###########INVERSIONES
#en principio 1 es malo y entre sube mejora
#a excepcion de democracia_21 donde 1 es prefereible democracia
#percepcion_3 esta ivnertida donde 1 es mejoria a futuro. invertirla para que valores mas bajos sean ams pesimiso
#hay que invertir percepcion_3 1=3, 2=2, 3=1, ignorar valores -8 y -9
df$percepcion_3 <- ifelse(df$percepcion_3 == 1, 3,
                          ifelse(df$percepcion_3 == 2, 2,
                                 ifelse(df$percepcion_3 == 3, 1, df$percepcion_3)))

#tambien invertir percepcion_4 que es eprcepcion de progresod e pais
df$percepcion_4 <- ifelse(df$percepcion_4 == 1, 3,
                          ifelse(df$percepcion_4 == 2, 2,
                                 ifelse(df$percepcion_4 == 3, 1, df$percepcion_4)))

#percepcion_38 que es situacion politica de chile esta invertida donde 1 es muy bueno
#hay que invertir percepcion_38, 1=5, 2=4, 3=3, 4=2, 5=1, ignorar valores -8 y -9
df$percepcion_38 <- ifelse(df$percepcion_38 == 1, 5,
                           ifelse(df$percepcion_38 == 2, 4,
                                  ifelse(df$percepcion_38 == 3, 3,
                                         ifelse(df$percepcion_38 == 4, 2,
                                                ifelse(df$percepcion_38 == 5, 1, df$percepcion_38)))))


#y percepcion_6 que es situacion personal economica a futuro esta donde 1 es mas optimismo
#hay que invetir percepcion_5 1=5, 2=4, 3=3, 4=2, 5=1, ignorar valores -8 y -9
df$percepcion_6 <- ifelse(df$percepcion_6 == 1, 5,
                           ifelse(df$percepcion_6 == 2, 4,
                                  ifelse(df$percepcion_6 == 3, 3,
                                         ifelse(df$percepcion_6 == 4, 2,
                                                ifelse(df$percepcion_6 == 5, 1, df$percepcion_6)))))

#Listas las inversiones

#borrar objeto base
rm(base)

#quiero generar matriz de correlaciones entre todas las percepcion, bienestar_2 y democracia_21. redondear resultado a 2 decimales}
#omitir calculo si valor es -8 o -9
cor_matrix <- round(cor(df[, c("percepcion_2", "percepcion_3", "percepcion_4", "percepcion_5", "percepcion_6", "percepcion_38", "democracia_21")], use="pairwise.complete.obs"), 2)
print(cor_matrix)

cor_confianza<- round(cor(df[, c("confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p")], use="pairwise.complete.obs"), 2)
print(cor_confianza)

rm(cor_matrix, cor_confianza)

#quiero ahcer una regresion lineal donde democracia_21 es la variable dependiente y percepcion_2 y percepcion_5 son predictores
model <- lm(democracia_21 ~ percepcion_2 + percepcion_5+percepcion_4+percepcion_3+gse, data=df) #situacion presente de economia pais y personal, futura eeconomica y personal
summary(model)
#situacion economica presente y futura del gobierno predicen, pero no la personal
#recordar que aca valroes ams bajos de democrcia_21 es mas apoyo a democracia. Entonces entre mas aumenta optimismo mas bajo valores en democracia_21, es decir mas apoyo a democracia
#y gse mas alto es ams pobre, entonces entre mas pobre menos apoyo

##########CONSTRUCCION DE EPRFILES LATENTES CON LAS VARIABLES DE PERCEPCION
set.seed(324)

#crear nuevo data frame llamado df_lpa que contenga solo las columnas percepcion_3, percepcion_2, percepcion_5, percepcion_6, percepcion_38
df_lpa <- df[, c("percepcion_3", "percepcion_2", "percepcion_5")]


lpa_n1<-estimate_profiles(df_lpa, 2:5,
                          return_posterior_prons=TRUE)
lpa_n1
rm(df_lpa, lpa_n1)
#pareciera que mejor es el de 5 clases

#lo calculare con error estandar y promedio robusto
mr<-covMcd(df_lpa)
mr$center #estimacion roubusta de vector promedio, mas apropiado que cov
#ya que lpa usa promedios



lpa_r<-scale(df_lpa, center=mr$center, scale=FALSE)

lpa_n2<-estimate_profiles(lpa_r, 2:4,
                          return_posterior_prons=T,
                          se.method="HC")
lpa_n2



lpa_r4<-estimate_profiles(df_lpa, 4,
                          return_posterior_prons=T,
                          se.method="HC")
summary(lpa_r4)

plot_profiles(lpa_r4)

profile_summary <- get_data(lpa_r4) #probabilidades de cada indiviudo de pertener a un perfil
plot_density(lpa_r4,
             variables=NULL,
             conditional=FALSE)


#Clase 1 parece ser pesimista en situacion economica pais pero moderados sobre personal (Yo bien)
#Clase 3 parace ser moderada en todo la evaluacion (Moderados)
#Clase 2 ees la mas optimista en todo (Optimistas)
#Clase 4 es pesimsita en todo (Pesimsitas)


table(profile_summary$Class)

#pasar a df
df$perfil<-profile_summary$Class

str(df)
df$perfil<-as.factor(df$perfil)
df$sexo<-as.factor(df$sexo)
df$gse<-as.factor(df$gse)

rm(df_lpa, lpa_n1, profile_summary)

#realizar anova usando democracia_21 como vd y perfil como predictor
anova_model <- aov(democracia_21 ~ perfil+gse, data = df)
summary(anova_model)

#como un 2% de varianza en apoyo a la democracia se explica por este moldeo

#hay diferencias significativas entre perfiles en apoyo a democracia
#realizar post hoc tukey con correcion de bonferroni
post_hoc <- TukeyHSD(anova_model, "perfil", conf.level = 0.95)
print(post_hoc)



#diferencias significativas 2 v1 y 4v2
#diferencias entre Optimistas y centradas en si  --->mas defensores de democracia
#y diferencias entre Optimsitas y pesimsitas --->mas defensores de democracia

#diferencias margilanes entre 3 y 2
#diferencias marginales entre moderados y Optimistas --->mas defensores de democracia

#ES DECIR, UNA SITUACION ECONOMICA PERSONAL MAS MEJRO NO PREDICE MAYOR APOYO A DEMOCRACIA
#ES LA SITUACION PAIS EN SU TOTALIDAD
#nO HAY DIFERENCIAS ENTRE MODERADOS Y PESIMISTAS POR EJEMPLO


post_hoc_gse <- TukeyHSD(anova_model, "gse", conf.level = 0.95)

print(post_hoc_gse)


#1 es mas rico 5 es mas pobre
#grupos 3 y 4 tienen significativamente menos apoyo a democracia que 1
#grupos 3 y 4 tienen significativamente menos apoyo a democracia que 1
#grupos 3 y 4 menos apoyo a democracia que 2
#marginal diferencia etnre 1 y 2
#faltas de diferencias de 5 puede ser por bajo numero de casos



table(df$gse, df$perfil)#el tema es que hay muy pocas personas del eprfil mas pobre

#quiero hacer un chisq.test de distribucion de "gse" segun perfil
chisq_test <- chisq.test(table(df$gse, df$perfil))
print(chisq_test)
#hay diferencias significativas en distribucion de gse segun perfil

#quiero abrir github y subir esta version

#podria ahcer lo mismo pero con confianza en instituciones



