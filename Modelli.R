##### 1. Organizzazione e creazione del dataset con tutti i paesi e anni #####

# Lista dei paesi da controllare


tutti_paesi_compatibili <-    #### tutti i paesi con numero di osservazioni 
                              #### adatto all'analisi
  c("Austria"    , "Belgium",
    "Cyprus"     ,"Estonia" ,
    "Finland"    , "France" ,
    "Germany"    ,"Greece"  ,
    "Ireland"    ,"Italy"   ,
    "Latvia"     ,"Lithuania",
    "Luxembourg" ,"Malta"  ,"Netherlands",
    "Portugal"   ,"Slovenia","Spain" )


paesi=tutti_paesi_compatibili


paesi <- 
  c( "Belgium",
     
     "Finland"    , "France" ,
     "Germany"    ,"Greece"  ,
     "Ireland"    ,"Italy"   ,
     "Latvia"     ,"Lithuania",
     "Luxembourg"   ,"Netherlands",
     "Portugal"   ,"Slovenia","Spain" )



paesi <- c("France" ,
           "Germany",
           "Italy" ,
           "Netherlands",
           "Spain",
           "Belgium") 

paesi = c("Italy","Spain")
#### selezione finale dei paesi secondo criterio GDP più grande e stabile




# Dataframe vuoto per unire tutti i dati
data_all_paesi <- data.frame()

# Ciclo per controllare ogni paese e selezionare le prime 32 osservazioni
for (paese in paesi) {
  
  # Creiamo un dataframe temporaneo per ogni paese, limitato a 32 osservazioni
  temp_df <- data.frame(
    Anno = na.omit(T00$DATE[T00[[paese]] > 0])[1:32],  # Colonna Anno (prime 32 osservazioni)
    T00 = na.omit(T00[[paese]])[1:32],                # Variabile T00 (prime 32 osservazioni)
    GDP = GDP[[paese]][1:32],                         # Variabile GDP (prime 32 osservazioni)
    HICP = HICP[[paese]][1:32],                       # Variabile HICP (prime 32 osservazioni)
    A10 = A10[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A20 = A20[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A30 = A30[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A40 = A40[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A50 = A50[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A60 = A60[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A70 = A70[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    A80 = A80[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    L20 = L20[[paese]][1:32],                         # Variabile L20 (prime 32 osservazioni)
    L30 = L30[[paese]][1:32],                         # Variabile L30 (prime 32 osservazioni)
    L40 = L40[[paese]][1:32],                         # Variabile L40 (prime 32 osservazioni)
    L50 = L50[[paese]][1:32],                         # Variabile L40 (prime 32 osservazioni)
    L51 = L51[[paese]][1:32],                         # Variabile L40 (prime 32 osservazioni)
    L70 = L70[[paese]][1:32],                         # Variabile L40 (prime 32 osservazioni)
    
        Paese = rep(paese, 32)                            # Nome del paese ripetuto per 32 osservazioni
  )
  
  # Uniamo questo dataframe temporaneo con il dataframe generale
  data_all_paesi <- rbind(data_all_paesi, temp_df)
}

# Visualizziamo i primi dati uniti
head(data_all_paesi)

##### 2. Modello GLM/lineare, tutti i paesi #####


model_glm <- glm(T00/L40 ~ HICP + L20 + L30 
                   +A20 + A30+A40+A50+A60+A70+A80+
                   factor(Anno)+ factor(Paese), 
                   data = data_all_paesi, 
                   family = Gamma(link = "log"))


# Riassunto del modello logit
summary(model_glm)
pscl::pR2(model_glm)
DescTools::PseudoR2(model_glm)

model_lm <- lm(T00/L40 ~ HICP + L20 + L30 
                 +A20 + A30+A40+A50+A60+A70+A80, 
                  factor(Anno)+ factor(Paese), 
                 data = data_all_paesi)

summary(model_lm) 
car::vif(model_lm)

##### 3. Modello Lineare (T00/L40) #####


summary(data_all_paesi$T00/data_all_paesi$L40)
hist(data_all_paesi$T00/data_all_paesi$L40, breaks = 20)

##### Hist T00/L40 per paese

par(mfrow=c(3,2))
for (paese in paesi) {  
  hist(T00[[paese]]/L40[[paese]],main = paese, breaks = 9)
}
##### Shapiro-test
results <- list() 
for (paese in paesi) {  
  
  temp1=T00[[paese]]
  temp2=L40[[paese]]
  
  shapiro=shapiro.test(temp1/temp2)
  
  
  shapiro_test_result <- shapiro
  
  # Store the results
  results[[paese]] <- shapiro$p.value
}

results ## Solo italia e Spagna distribuzione normale


##### Modelli lineari Spagna #####

full_model_spain =lm(T00/L40 ~ 
                HICP + L20 
                +L30 + L50
                +L70 +L51
                +GDP 
                +A20 + A30
                +A40 + A50
                +A60 + A70
                +A80, 
                data_all_paesi[data_all_paesi$Paese== "Spain",])
                
summary(full_model_spain) 
car::vif(full_model_spain)

model_stepwise_spain <- step(full_model_spain, direction = "both")
summary(model_stepwise_spain)
car::vif(model_stepwise_spain)

model_spain =lm(T00/L40 ~ 
                     +L50
                     +A30
                     +A40 
                     +A80, 
                     data_all_paesi[data_all_paesi$Paese== "Spain",])
summary(model_spain) 
car::vif(model_spain)



shapiro.test(model_stepwise_spain$residuals)
lmtest::bptest(model_stepwise_spain)
lmtest::dwtest(model_stepwise_spain)


shapiro.test(model_spain$residuals)
lmtest::bptest(model_spain)
lmtest::dwtest(model_spain)
#### assunzioni verificate per entrambi i modelli

##### Modelli lineari Italia #####

full_model_italy =lm(T00/L40 ~ 
                       HICP + L20 
                     +L30 + L50
                     +L70 
                     +GDP 
                     +A20 + A30
                     +A40 + A50
                     +A60 + A70
                     +A80, 
                data_all_paesi[data_all_paesi$Paese== "Italy",])

summary(full_model_italy) 
car::vif(full_model_italy)


model_stepwise_italy <- step(full_model_italy, direction = "both")
summary(model_stepwise_italy)
car::vif(model_stepwise_italy)

model_italy =lm(T00/L40 ~ 
                +L20
                +L50
                +A30
                +A60,
                data_all_paesi[data_all_paesi$Paese== "Italy",])

summary(model_italy) 
car::vif(model_italy)


#### Multicollinearità ridotta rispetto modello stepwise
#### e significatività di tutte le variabili, leggera riduzione
#### di R^2 corretto


shapiro.test(model_stepwise_italy$residuals)
lmtest::bptest(model_stepwise_italy)
lmtest::dwtest(model_stepwise_italy)


shapiro.test(model_italy$residuals)
lmtest::bptest(model_italy)
lmtest::dwtest(model_italy)

#### assunzioni verificate per entrambi i modelli

  

########


# Calcolare T00/L40 e aggiungerlo al dataframe
data_all_paesi <- data_all_paesi %>%
  mutate(ratio_T00_L40 = T00 / L40)  # Crea una nuova colonna per il rapporto

# Ottenere il summary per ogni paese
summary_by_country <- data_all_paesi %>%
  group_by(Paese) %>%
  summarise(
    Mean = mean(ratio_T00_L40, na.rm = TRUE),         # Media
    Median = median(ratio_T00_L40, na.rm = TRUE),     # Mediana
    SD = sd(ratio_T00_L40, na.rm = TRUE),              # Deviazione standard
    Min = min(ratio_T00_L40, na.rm = TRUE),            # Minimo
    Max = max(ratio_T00_L40, na.rm = TRUE),            # Massimo
    Count = n()                                        # Conteggio dei valori
  )

# Mostrare il risultato
print(summary_by_country)


summary(data_all_paesi$T00/(data_all_paesi$L40+
                      data_all_paesi$L20+
                        data_all_paesi$L30))


##### GG_PLOT Line T00/L40 paesi Analisi #####

ggplot(data_all_paesi, aes(x = Anno, y = T00_L40_ratio, color = Paese)) +
  geom_line() +  # Aumenta lo spessore delle linee
  geom_point(size = 2) +   # Aumenta la dimensione dei punti
  labs(
    title = "Rapporto T00/L40 per Paese nel Tempo",
    x = "Anno",
    y = "Rapporto T00/L40",
    color = "Paese"
  ) +
  theme_minimal()


# Calcola il rapporto T00/L40 e aggiungilo al dataframe
data_all_paesi <- data_all_paesi %>%
  mutate(ratio = T00 / L40)

# Crea l'istogramma
ggplot(data_all_paesi, aes(x = reorder(Paese, ratio), y = ratio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Ruota l'asse X e Y per facilitare la lettura
  labs(title = "Rapporto T00/L40 per Paese",
       x = "Paese",
       y = "Rapporto T00/L40") +
  theme_minimal()


# Installare ggplot2 se non è già installato
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Filtra i dati per l'anno 2023
data_2023 <- data_all_paesi %>%
  filter(format(Anno, "%Y") == "2023") %>%  # Assicurati di filtrare per l'anno 2023
  mutate(ratio = T00 / L40)  # Calcola il rapporto T00/L40

# Visualizza i valori osservati
print(data_2023 %>% select(Paese, ratio))

# Crea l'istogramma
ggplot(data_2023, aes(x = reorder(Paese, ratio), y = ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Rapporto T00/L40 per Paese nel 2023",
       x = "Paese",
       y = "Rapporto T00/L40") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(data_2023)

# Installare ggplot2 se non è già installato
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Filtra i dati per l'anno 2023
data_2023 <- data_all_paesi %>%
  filter(format(Anno, "%Y") == "2023") %>%  # Assicurati di filtrare per l'anno 2023
  mutate(ratio = T00 / (L40+L30+L20))  # Calcola il rapporto T00/L40

# Visualizza i valori osservati
print(data_2023 %>% select(Paese, ratio))

# Crea l'istogramma
ggplot(data_2023, aes(x = reorder(Paese, ratio), y = ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Rapporto T00/L40 per Paese nel 2023",
       x = "Paese",
       y = "Rapporto T00/L40 (Valori Effettivi)") +
  geom_text(aes(label = round(ratio, 2)), vjust = -0.5)  # Aggiungi i valori effettivi come etichette sopra le barre
summary(data_2023)


library(dplyr)

# Riepilogo del GDP per ogni paese e ordinamento
summary_gdp <- data_all_paesi %>%
  group_by(Paese) %>%
  summarise(
    mean_GDP = mean(GDP, na.rm = TRUE),
    median_GDP = median(GDP, na.rm = TRUE),
    min_GDP = min(GDP, na.rm = TRUE),
    max_GDP = max(GDP, na.rm = TRUE),
    sd_GDP = sd(GDP, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(mean_GDP))  # Ordina per media del GDP in ordine decrescente

print(summary_gdp)




# Calcola il rapporto GDP e aggiungi una nuova colonna
data_all_paesi <- data_all_paesi %>%
  mutate(T00_GDP_ratio = GDP) %>%
  filter(!is.na(T00_GDP_ratio))  # Rimuovi eventuali righe con NA

# Crea un grafico per visualizzare il GDP per tutti i paesi nel tempo
ggplot(data_all_paesi, aes(x = Anno, y = T00_GDP_ratio, color = Paese)) +
  geom_line(size = 1.2) +  # Usa linee più spesse
  geom_point(size = 2) +   # Aggiungi punti più grandi ai dati
  labs(
    title = "GDP per Paese nel Tempo",
    x = "Anno",
    y = "GDP",
    color = "Paese"
  ) +
  theme_minimal() +  # Usa un tema minimalista per il grafico
  theme(
    legend.position = "right",  # Posiziona la legenda a destra
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Set1") +  # Cambia la palette di colori
  geom_text(aes(label = Paese), 
            hjust = 1.5, 
            vjust = -0.5, 
            size = 3, 
            check_overlap = TRUE)  # Aggiungi le etichette dei paesi

library(dplyr)

# Riassumi il GDP per paese e anno, e ordina per GDP decrescente
summary_gdp <- data_all_paesi %>%
  group_by(Paese) %>%  # Raggruppa per Paese e Anno
  summarise(Total_GDP = sum(GDP, na.rm = TRUE)) %>%  # Calcola la somma del GDP
  arrange(desc(Total_GDP))  # Ordina per GDP decrescente

# Visualizza il riassunto
print(summary_gdp)

# Assicurati di avere ggplot2 installato e caricato
install.packages("ggplot2")  # Se non l'hai già installato
library(ggplot2)


# Crea un istogramma del GDP per paese nel 2023
ggplot(data_2023, aes(x = reorder(Paese, GDP), y = GDP)) +
  geom_bar(stat = "identity", fill= "steelblue") +
  labs(title = "GDP per Paese nel 2023",
       x = "Paese",
       y = "GDP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostra un riepilogo dei dati per il 2023
summary(data_2023)
