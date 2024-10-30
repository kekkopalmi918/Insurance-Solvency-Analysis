##### Inserimento dati ####
setwd("C:/")
library(readr)
library(MASS)

##### Functions #####
rinomina <- function(S128_T00) {
  # Use gsub to extract only the country names from the column names
  new_colnames <- gsub(".*reported.by.insurance.corporations.in.|..stocks.*", "", colnames(S128_T00))
  
  # Set the first two column names back to "DATE" and "TIME.PERIOD" as they are not country names
  new_colnames[1:2] <- c("DATE", "TIME.PERIOD")
  
  # Apply the new column names to the dataset
  colnames(S128_T00) <- new_colnames
  
  # Return the modified dataset
  return(S128_T00)
}
read_csv_liste=function(lista_file){
  for (file in lista_file) {
    
    # Rimuove l'estensione '.csv' per usare come nome del dataframe
    nome_df <- gsub(".csv", "", file)
    
    # Legge il file e lo assegna a una variabile con il nome dinamico
    assign(nome_df, read_csv(file),envir = .GlobalEnv)
    
    # Stampa il nome del dataframe creato come feedback
    print(paste("Creato dataframe:", nome_df))
  }
}
rinomina_colonne <- function(df) {
  # Estrarre i nomi delle colonne
  col_names <- colnames(df)
  
  # Modifica dei nomi delle colonne per mantenere solo i nomi delle nazioni
  col_names_modificati <- gsub(".*?\\s+([A-Za-z]+)\\s*\\(.*", "\\1", col_names)
  
  # Imposta i nuovi nomi delle colonne al dataframe
  colnames(df) <- col_names_modificati
  
  return(df)
}

##### Files #####
lista_file=list.files(pattern = "\\.csv")
read_csv_liste(lista_file)

lista_dataframe=gsub(lista_file,pattern = '\\.csv'  ,replacement = '' )


# Applica la funzione a tutti i dataframe nella lista
for (dataframe_name in lista_dataframe) {
  
  # Ottieni il dataframe reale dal nome
  dataframe <- get(dataframe_name)
  
  # Applica la funzione per uniformare le colonne
  dataframe <- rinomina_colonne(dataframe)
  
  # Salva il dataframe modificato con lo stesso nome
  assign(dataframe_name, dataframe)
}

# Slovakia poche osservazioni
A80$Slovakia=NULL

# Rimozione righe con NA
for (dataframe_name in lista_dataframe) {
  
  # Ottieni il dataframe reale dal nome
  dataframe <- get(dataframe_name)
  
  # Rimozione righe con NA
  dataframe=dataframe[complete.cases(dataframe),]
  
  # Salva il dataframe modificato con lo stesso nome
  assign(dataframe_name, dataframe)
}


######## RINOMINA E CARICA GDP ######
# Estrarre i codici paese dalle colonne
# 
GDP <- read_csv("GDP.csv")
GDP= GDP[269:302,]
new_colnames <- gsub(".*MNA\\.Q\\.Y\\.|\\.W2.*", "", colnames(GDP))

# Creare una mappatura tra i codici e i nomi dei paesi
mappatura_paesi <- c(
  AT = "Austria", BE = "Belgium", CY = "Cyprus", DE = "Germany", EE = "Estonia", 
  ES = "Spain", FI = "Finland", FR = "France", GB = "United Kingdom", GR = "Greece", 
  IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", MT = "Malta",
  NL = "Netherlands", PT = "Portugal", SI = "Slovenia", IE = "Ireland", HR = "Croatia",
  BG = "Bulgaria", CZ = "Czech Republic", HU = "Hungary", PL = "Poland", RO = "Romania", 
  SE = "Sweden", SK = "Slovakia", DK = "Denmark"
)

# Rinominare i codici paese con i nomi completi
new_colnames <- mappatura_paesi[new_colnames]

# Mantenere le prime due colonne originali (DATE e TIME PERIOD)
new_colnames[1:2] <- c("DATE", "TIME PERIOD")

# Assegnare i nuovi nomi delle colonne al dataframe
colnames(GDP) <- new_colnames

# Controllare i nuovi nomi delle colonne
colnames(GDP)

GDP <- GDP[, !is.na(colnames(GDP))]
length(GDP$DATE)
GDP=GDP[3:34,]

#####
#####
##### RINOMINA HICP #####
HICP=read_csv("HICP.csv")
HICP= HICP[343:441,]
HICP= HICP[seq(0,99,by=3),]
# Estrazione dei codici paese dalle colonne HICP
new_colnames <- gsub(".*ICP\\.M\\.|\\.N.*", "", colnames(HICP))

# Creazione della mappatura tra codici e nomi dei paesi
mappatura_paesi <- c(
  AT = "Austria", BE = "Belgium", CY = "Cyprus", DE = "Germany", EE = "Estonia",
  ES = "Spain", FI = "Finland", FR = "France", GB = "United Kingdom", GR = "Greece",
  IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", MT = "Malta",
  NL = "Netherlands", PT = "Portugal", SI = "Slovenia", IE = "Ireland", HR = "Croatia",
  BG = "Bulgaria", CZ = "Czech Republic", HU = "Hungary", PL = "Poland", RO = "Romania",
  SE = "Sweden", SK = "Slovakia", DK = "Denmark"
)

# Rinominazione dei codici paese con i nomi completi
new_colnames <- mappatura_paesi[new_colnames]

# Mantenere le prime due colonne originali (DATE e TIME PERIOD)
new_colnames[1:2] <- c("DATE", "TIME PERIOD")

# Assegnare i nuovi nomi delle colonne al dataframe
colnames(HICP) <- new_colnames

# Controllare i nuovi nomi delle colonne
colnames(HICP)

HICP <- HICP[, !is.na(colnames(HICP))]
colnames(HICP)
#####
##### Grafici T00 ####
Germania = na.omit(T00$Germany)[1:32]
Italia = na.omit(T00$Italy)[1:32]
Francia = na.omit(T00$France)[1:32]
Croazia = na.omit(T00$Croatia)[1:32]
Finlandia = na.omit(T00$Finland)[1:32]
Spagna = na.omit(T00$Spain)[1:32]

lista_paesi <- list(Germania = Germania, Italia = Italia, Francia = Francia, Croazia = Croazia, Finlandia = Finlandia, Spagna = Spagna)

par(mfrow=c(2,3))
# Ciclo for per plottare i dati
for (paese in names(lista_paesi)) {
  dati_paese <- lista_paesi[[paese]]
  
  # Creazione del grafico
  plot(dati_paese, type = "l", col = "blue", lwd = 2, 
       xlab = "Time", ylab = "Value", main = paste("Trend Line for", paese))
  
  # Aggiunta della linea di trend
  abline(lm(dati_paese ~ seq_along(dati_paese)), col = "red", lwd = 2)
}

# Ciclo for per hist paese
par(mfrow=c(2,3))
for (paese in names(lista_paesi)) {
  dati_paese <- lista_paesi[[paese]]
  
  # Creazione del grafico
  hist(dati_paese, xlab = paese)
}


##### Grafici L40 #####


Germania = na.omit(L40$Germany)[1:32]
Italia = na.omit(L40$Italy)[1:32]
Francia = na.omit(L40$France)[1:32]
Croazia = na.omit(L40$Croatia)[1:32]
Finlandia = na.omit(L40$Finland)[1:32]
Spagna = na.omit(L40$Spain)[1:32]

lista_paesi_L40 <- list(Germania = Germania, Italia = Italia, Francia = Francia, Croazia = Croazia, Finlandia = Finlandia, Spagna = Spagna)


par(mfrow=c(2,3))
# Ciclo for per plottare i dati
for (paese in names(lista_paesi_L40)) {
  dati_paese <- lista_paesi_L40[[paese]]
  
  # Creazione del grafico
  plot(dati_paese, type = "l", col = "blue", lwd = 2, 
       xlab = "Time", ylab = "Value", main = paste("Trend Line for", paese))
  
  # Aggiunta della linea di trend
  abline(lm(dati_paese ~ seq_along(dati_paese)), col = "red", lwd = 2)
}

# Ciclo for per hist paese
par(mfrow=c(2,3))
for (paese in names(lista_paesi_L40)) {
  dati_paese <- lista_paesi_L40[[paese]]
  
  # Creazione del grafico
  hist(dati_paese, xlab = paese)
}




#####

# Riassunto dei risultati della regressione

model=lm(Italia[1:14] ~ GDP$Italy[1:14] + HICP$Italy[1:14], )
summary(model)
model=lm(Italia[21:32] ~ GDP$Italy[21:32] + HICP$Italy[21:32], )
summary(model)
par(mfrow=c(2,2))
plot(GDP$Italy[1:32], type = "l")
plot(GDP$Italy[1:14], type = "l")
plot(GDP$Italy[14:21], type = "l")
plot(GDP$Italy[21:32], type = "l")


date = na.omit(T00$DATE[T00$Italy>0])[1:32]
date[14]

par(mfrow=c(1,1))
plot(GDP$Italy,type = "l", col = "red")

temp=L20$ItalystocksICBQITXS128L20T1W0S1_TEUR+L30$ItalystocksICBQITXS128L30T1W0S1_TEUR+L40$ItalystocksICBQITXS128L40T1W0S1_TEUR+L50$ItalystocksICBQITXS128L50T1W0S1_TEUR
T00$ItalystocksICBQITXS128T00T1W0S1_TEUR/temp

temp=L20$FrancestocksICBQFRXS128L20T1W0S1_TEUR+L30$FrancestocksICBQFRXS128L30T1W0S1_TEUR+L40$FrancestocksICBQFRXS128L40T1W0S1_TEUR+L50$FrancestocksICBQFRXS128L50T1W0S1_TEUR
T00$FrancestocksICBQFRXS128T00T1W0S1_TEUR/temp

plot(T00$ItalystocksICBQITXS128T00T1W0S1_TEUR, type="l")


shapiro.test(Italia)
shapiro.test(Francia)
shapiro.test(Germania)
shapiro.test(Croazia)
shapiro.test(Finlandia)

##### test distribuzione gamma ####
test_gamma <- function(data) {
  fit <- fitdistr(data, "gamma")
  breaks <- hist(data, plot = FALSE)$breaks
  observed <- hist(data, breaks = breaks, plot = FALSE)$counts
  expected <- diff(pgamma(breaks, shape = fit$estimate['shape'], rate = fit$estimate['rate'])) * length(data)
  chisq_test <- chisq.test(observed, p = expected/sum(expected))
  return(chisq_test)
}

# Esegui il test gamma per ciascuna variabile
test_italia <- test_gamma(Italia)
test_francia <- test_gamma(Francia)
test_germania <- test_gamma(Germania)
test_croazia <- test_gamma(Croazia)
test_finlandia <- test_gamma(Finlandia)
test_estonia <- test_gamma(T00$Estonia)
test_spagna <- test_gamma(T00$Spain)


test_germania
test_francia
test_italia
test_croazia
test_estonia
test_finlandia
test_spagna
