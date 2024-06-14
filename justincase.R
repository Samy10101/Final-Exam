---
  title: "Examen Final"
author: "Jean Samy Francois et Israel Jean Francois"
date: "2024-06-13"
output: word_document 
---
  
  # Choix des variables et explications
  
  
  
  
  # Importations de données 
  
  ```{r setup, include=FALSE}

taux_change <- wb_data(indicator = "PA.NUS.FCRF", country = "HTI", start_date = 1988, end_date = 2021)

exportations<- wb_data(indicator= "NE.EXP.GNFS.ZS" , country = "HTI" , start_date = 1988, end_date = 2021)

importations<- wb_data(indicator = "NE.IMP.GNFS.CD", country = "HTI", start_date = 1988, end_date = 2021)

IDE<- wb_data(indicator="BX.KLT.DINV.WD.GD.ZS", country = "HTI", start_date= 1988, end_date = 2021)

Inflation<- wb_data(indicator="FP.CPI.TOTL.ZG", country="HTI", start_date= 1988, end_date = 2021)

croiss_eco<- wb_data(indicator="NY.GDP.MKTP.KD.ZG", country="HTI",start_date= 1988, end_date = 2021)  

# Nettoyage des tableaux
taux_change<- taux_change[, c(3, 4, 5)]
exportations<- exportations[, c(5)]
importations<- importations[, c(5)]
IDE<- IDE[, c(5)]
Inflation<- Inflation[, c(5)]
Croiss_eco <- croiss_eco[, c(5)]  


# Tableaux combinés
tableau_combine1<- cbind(taux_change, exportations,importations,IDE,Inflation,croiss_eco)

# Renommons les variables
colnames(tableau_combine)<- c("Pays", "Date", "Tauxdechange", "Exportations", "Importations", "IDE","taux d'inflation","croissance economique")
tableau_combine<-tableau_combine[,c(1,2,3,4,5,6,7,12)]
tableau_combine
colnames(tableau_combine)<- c("Pays", "Date", "Tauxdechange", "Exportations", "Importations", "IDE","inflation","croissance")

# Régression linéaire 

reg_lin<-summary(lm(Tauxdechange~Exportations+Importations+IDE+inflation+croissance,tableau_combine))
reg_lin

```


## Tableau des variables 

===============================================================================
  
  ### Tableau 1
  ```{r echo=FALSE}
print(tableau_combine)
```


Nuage de points
===============================================================================
  
  # Représentation grapique
  
  ### graphe de la regression des exportations sur le taux de change
  
  ```{r echo=FALSE}
ggplot(data=tableau_combine, mapping=aes(x=Exportations,y=Tauxdechange)) + geom_point()+
  labs(title = "Nuage de points|Exportaions ")+geom_smooth(method = "lm")
```


### graphe de la régression de la croissance du PIB sur le Taux de change
```{r}
ggplot(data=tableau_combine, mapping=aes(x=croissance,y=Tauxdechange)) + geom_point()+
  labs(title = "Nuage de points|croissance ")+geom_smooth(method = "lm")

```


### graphe de la régression des importations sur le Taux de change

```{r echo=FALSE}
ggplot(data=tableau_combine, mapping=aes(x=Importations,y=Tauxdechange)) + geom_point()+
  labs(title = "Nuage de points|Inportaions ")+geom_smooth(method = "lm")
```

### graphe de la régression de l'inflation des prix à la consommation sur le Taux de change

```{r echo=FALSE}
ggplot(data=tableau_combine, mapping=aes(x=inflation ,y=Tauxdechange)) + geom_point()+
  labs(title = "Nuage de points|inflation ")+geom_smooth(method = "lm")
```


###graphe de la régression de l'IDE sur le Taux de change

```{r echo=FALSE}
ggplot(data=tableau_combine, mapping=aes(x=IDE,y=Tauxdechange)) + geom_point()+
  labs(title = "Nuage de points|IDE ")+geom_smooth(method = "lm")

```



# Résultats de la régression linéaire

Dans le tableau suivant se trouve les resultats de la régression des exportations, des importations, de l'inflation des prix à la consommation, de la Croissance du PIB et de l'IDE sur le Taux de Change: 
  
  ```{r echo=FALSE}
Mod_reg <- lm(Tauxdechange~inflation + Importations + Exportations + croissance + IDE, data = tableau_combine)
Stat_01<- data.frame(summary(Mod_reg)$coefficients[,c(1,4)])
R.Squared <- summary(Mod_reg)$r.squared
adj.Rsquared <- summary(Mod_reg)$adj.r.squared
gg_stat01 <- summary(Mod_reg)$fstatistic[1]
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],f[4],f[5],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
p_value <- lmp(Mod_reg)
Stat_description <- c("R_carré","R2.ajusté","Stat_Fisher","P_Value")
Stat_03 <- c(R.Squared,adj.Rsquared,gg_stat01,p_value)
Stat_02 <- data.frame(Stat_description,Stat_03)
tableau2 <- data.frame(Stat_01,Stat_02) 
tableau2

```





### Nuage de points valeurs résiduelles vs valleurs estimées 

graphique contenant les valeurs résiduelles de la régression et les valeurs estimées

```{r echo=FALSE}
resid <- residuals(Mod_reg)
fitt_val <- fitted.values(Mod_reg)
df_resid <- data.frame(resid, fitt_val)
plot(df_resid$fitt_val,df_resid$resid, pch=20, main='Analyse des résidus',
     xlab='Valeurs estimées', ylab='Erreurs résiduelles')
abline(lm(resid ~ fitt_val), col='red', lwd=2)
```



### Commentaires
```{r echo=FALSE}

```
