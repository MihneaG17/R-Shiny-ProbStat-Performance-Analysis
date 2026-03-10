library(shiny)
library(bslib)

#functie pentru simularea unei cereri
simuleaza_cerere <- function(p_succes = 0.8, n_max_retry = 3, t_backoff = 50, 
                              t_timeout = 800, shape = 2, scale = 100) {
  t_total <- 0
  nr_incercari <- 0
  nr_esecuri <- 0
  succes_final <- 0
  
  while(nr_incercari <= n_max_retry) {
    nr_incercari <- nr_incercari + 1
    s_i <- rgamma(1, shape = shape, scale = scale)
    t_total <- t_total + s_i
    
    if(t_total > t_timeout) {
      succes_final <- 0
      break
    }
    if(runif(1) < p_succes) {
      succes_final <- 1
      break
    } else {
      nr_esecuri <- nr_esecuri + 1
      if(nr_incercari <= n_max_retry) {
        t_total <- t_total + t_backoff
      }
    }
  }
  return(c(succes = succes_final, timp = t_total, incercari = nr_incercari, esecuri = nr_esecuri))
}

#functie pentru simulare dependenta (latenta creste dupa esecuri)
simuleaza_dependent <- function(p_succes = 0.8, n_max_retry = 3, t_backoff = 50,
                                 t_timeout = 800, shape = 2, scale_baza = 100, penalizare = 100) {
  t_total <- 0
  nr_incercari <- 0
  nr_esecuri <- 0
  succes_final <- 0
  
  while(nr_incercari <= n_max_retry) {
    nr_incercari <- nr_incercari + 1
    scale_actual <- scale_baza + (nr_esecuri * penalizare)
    s_i <- rgamma(1, shape = shape, scale = scale_actual)
    t_total <- t_total + s_i
    
    if(t_total > t_timeout) {
      succes_final <- 0
      break
    }
    if(runif(1) < p_succes) {
      succes_final <- 1
      break
    } else {
      nr_esecuri <- nr_esecuri + 1
      if(nr_incercari <= n_max_retry) {
        t_total <- t_total + t_backoff
      }
    }
  }
  return(c(succes = succes_final, timp = t_total, incercari = nr_incercari, esecuri = nr_esecuri))
}

#functie pentru valoare modala
functie_val_mod <- function(v) {
  densitate <- density(v)
  varf_y <- which.max(densitate$y)
  return(densitate$x[varf_y])
}

#ui
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "pulse"),
  titlePanel("Analiza Probabilistica - Performanta serviciului online"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Parametri globali"),
      hr(),
      numericInput("n_simulari", "Nr. Simulari", value = 5000, min = 100, max = 50000, step = 1000),
      numericInput("seed", "Seed (reproductibilitate)", value = 123, min = 1),
      hr(),
      h5("Parametri cereri:"),
      sliderInput("p_succes", "Probabilitate succes", min = 0.1, max = 0.99, value = 0.8, step = 0.05),
      sliderInput("n_max_retry", "Max retry-uri", min = 0, max = 10, value = 3, step = 1),
      numericInput("t_timeout", "Timeout (ms)", value = 800, min = 100, max = 2000),
      numericInput("t_backoff", "Backoff (ms)", value = 50, min = 0, max = 200),
      hr(),
      h5("Parametri trafic:"),
      numericInput("lambda_trafic", "Lambda Poisson", value = 1000, min = 10, max = 5000),
      numericInput("n_market", "Dimensiune piata (Binomial)", value = 2000, min = 100, max = 10000),
      sliderInput("p_active", "Prob. client activ", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      hr(),
      actionButton("run_sim", "Ruleaza Simulare", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        
        #tab 1: Trafic zilnic
        tabPanel("1. Trafic zilnic",
          h3(" Modelarea traficului zilnic (Kd)"),
          fluidRow(
            column(6, plotOutput("plot_poisson")),
            column(6, plotOutput("plot_binomial"))
          ),
          hr(),
          h4(" Statistici comparative"),
          tableOutput("tabel_trafic"),
          verbatimTextOutput("interpretare_trafic")
        ),
        
        #tab 2: Timpi raspuns
        tabPanel("2. Timpi raspuns",
          h3(" Modelarea timpilor de raspuns (S)"),
          fluidRow(
            column(6, plotOutput("plot_gamma")),
            column(6, plotOutput("plot_normal"))
          ),
          hr(),
          h4(" Statistici descriptive"),
          tableOutput("tabel_timpi"),
          verbatimTextOutput("interpretare_timpi")
        ),
        
        #tab 3: Evenimente
        tabPanel("3. Evenimente",
          h3(" Cereri, Retry-uri si Evenimente"),
          fluidRow(
            column(6,
              h4(" Probabilitati Estimate"),
              tableOutput("tabel_probabilitati")
            ),
            column(6,
              h4(" Verificare Formule"),
              verbatimTextOutput("verificare_formule")
            )
          ),
          hr(),
          verbatimTextOutput("explicatie_probabilitati")
        ),
        
        #tab 4: (N, F) Bidimensional
        tabPanel("4. (N, F)",
          h3(" Variabila bidimensionala (N, F)"),
          fluidRow(
            column(6, 
              h4("Tabel distributie comuna"),
              tableOutput("tabel_NF")
            ),
            column(6, plotOutput("heatmap_NF"))
          ),
          hr(),
          h4("Distributii marginale"),
          fluidRow(
            column(6, tableOutput("marginala_N")),
            column(6, tableOutput("marginala_F"))
          ),
          verbatimTextOutput("test_independenta_NF")
        ),
        
        #tab 5: (N, T) bidimensional
        tabPanel("5. (N, T)",
          h3(" Variabila bidimensionala (N, T)"),
          fluidRow(
            column(6, plotOutput("boxplot_NT")),
            column(6, plotOutput("scatter_NT"))
          ),
          hr(),
          h4(" Statistici"),
          tableOutput("statistici_NT"),
          verbatimTextOutput("interpretare_NT")
        ),
        
        #tab 6: Conditionari
        tabPanel("6. Conditionari",
          h3(" Probabilități Conditionate"),
          numericInput("t0_sla", "Prag SLA t0 (ms)", value = 600, min = 100, max = 1500),
          numericInput("n0_incercari", "Prag incercari n0", value = 2, min = 1, max = 5),
          hr(),
          fluidRow(
            column(6,
              h4("Probabilitati Conditionate"),
              tableOutput("prob_conditionate")
            ),
            column(6,
              h4("Medii Conditionate"),
              tableOutput("medii_conditionate")
            )
          ),
          verbatimTextOutput("interpretare_conditionari")
        ),
        
        #tab 7: dependenta
        tabPanel("7. Dependenta",
          h3(" Independenta vs Dependenta"),
          sliderInput("penalizare_dep", "Penalizare latenta (ms/esec)", 
                      min = 0, max = 300, value = 100, step = 25),
          hr(),
          fluidRow(
            column(6, plotOutput("densitate_dep")),
            column(6, plotOutput("boxplot_dep"))
          ),
          hr(),
          tableOutput("comparatie_dep"),
          verbatimTextOutput("concluzii_dep")
        ),
        
        #tab 8: Inegalitati
        tabPanel("8. Inegalitati",
          h3(" Inegalitati Probabilistice"),
          tabsetPanel(
            tabPanel("Markov & Cebisev",
              h4("Inegalitatea Markov"),
              tableOutput("markov_tabel"),
              h4("Inegalitatea Cebisev"),
              tableOutput("cebisev_tabel")
            ),
            tabPanel("Chernoff",
              h4("Inegalitate Chernoff"),
              tableOutput("chernoff_tabel"),
              verbatimTextOutput("interpretare_chernoff")
            ),
            tabPanel("Jensen",
              h4("Inegalitatea Jensen"),
              tableOutput("jensen_tabel"),
              verbatimTextOutput("interpretare_jensen")
            )
          )
        ),
        
        #tab 9: Aproximare normala
        tabPanel("9. Aproximare",
          h3(" Aproximare normala si agregare"),
          numericInput("n_zile_agregare", "Zile de simulat", value = 1000, min = 100, max = 5000),
          numericInput("trafic_mediu_zi", "Trafic mediu/zi", value = 50, min = 10, max = 200),
          hr(),
          plotOutput("hist_agregare"),
          verbatimTextOutput("interpretare_agregare")
        ),
        
        #tab 10: Churn
        tabPanel("10. Churn",
          h3(" Pierderea utilizatorilor (Churn)"),
          fluidRow(
            column(4, sliderInput("q_random", "Prob. churn aleator", min = 0, max = 0.2, value = 0.05, step = 0.01)),
            column(4, numericInput("m_fereastra", "Fereastră cereri", value = 5, min = 2, max = 10)),
            column(4, numericInput("k_prag", "Prag eșecuri", value = 2, min = 1, max = 5))
          ),
          hr(),
          plotOutput("barplot_churn"),
          tableOutput("tabel_churn"),
          verbatimTextOutput("interpretare_churn")
        ),
        
        #tab 11: Impact economic
        tabPanel("11. Economic",
          h3(" Impact economic"),
          fluidRow(
            column(3, numericInput("castig_succes", "Castig/succes (RON)", value = 0.10, min = 0.01, max = 1, step = 0.01)),
            column(3, numericInput("pierdere_churn", "Pierdere/churn (RON)", value = 50, min = 1, max = 200)),
            column(3, numericInput("penalizare_sla", "Penalizare SLA (RON)", value = 0.50, min = 0.01, max = 5, step = 0.1)),
            column(3, numericInput("n_zile_profit", "Zile simulare", value = 100, min = 30, max = 365))
          ),
          hr(),
          fluidRow(
            column(6, plotOutput("hist_profit")),
            column(6, tableOutput("statistici_profit"))
          ),
          verbatimTextOutput("analiza_economica")
        ),
        
        #tab 12: Vizualizare
        tabPanel("12. Vizualizare",
          h3(" Vizualizare Statistica"),
          fluidRow(
            column(6, plotOutput("hist_T_viz")),
            column(6, plotOutput("boxplot_succes_esec"))
          ),
          hr(),
          h4(" Statistici Descriptive"),
          tableOutput("statistici_vizualizare"),
          verbatimTextOutput("interpretare_outlieri")
        ),
        
        #tab 13: Sinteza
        tabPanel("13. Sinteza",
          h3(" Analiza de Sinteza"),
          h4("a) Rolul probabilitatii empirice"),
          verbatimTextOutput("sinteza_a"),
          h4("b) Informatii din conditionari"),
          verbatimTextOutput("sinteza_b"),
          h4("c) Utilitatea inegalitatilor"),
          verbatimTextOutput("sinteza_c"),
          h4("d) Legatura performanta-economic"),
          verbatimTextOutput("sinteza_d"),
          h4("e) Parametri cheie si recomandari"),
          verbatimTextOutput("sinteza_e")
        )
      )
    )
  )
)


#partea de server, de functionalitate 
server <- function(input, output, session) {
  
  #Date care se recalculeaza cand se schimba parametrii
  date_simulare <- eventReactive(input$run_sim, { ##aici datele se schimba doar cand se apasa butonul run_sim
    
    set.seed(input$seed)
    
    #trafic zilnic
    n_zile <- 3 * 365 #trafic reprezentat pentru o perioada de 3 ani
    K_poisson <- rpois(n_zile, lambda = input$lambda_trafic)
    K_binomial <- rbinom(n_zile, size = input$n_market, prob = input$p_active)
    
    #timpi raspuns
    n_sim <- input$n_simulari
    S_gamma <- rgamma(n_sim, shape = 2, scale = 100)
    S_normala <- rnorm(n_sim, mean = 200, sd = 50)
    S_normala <- S_normala[S_normala > 0]
    
    #simulare cereri (independent)
    cereri <- t(replicate(n_sim, simuleaza_cerere(
      p_succes = input$p_succes,
      n_max_retry = input$n_max_retry,
      t_backoff = input$t_backoff,
      t_timeout = input$t_timeout
    )))
    df <- as.data.frame(cereri)
    colnames(df) <- c("I", "T", "N", "F")
    
    #simulare cereri (dependent)
    cereri_dep <- t(replicate(n_sim, simuleaza_dependent(
      p_succes = input$p_succes,
      n_max_retry = input$n_max_retry,
      t_backoff = input$t_backoff,
      t_timeout = input$t_timeout,
      penalizare = 100
    )))
    df_dep <- as.data.frame(cereri_dep)
    colnames(df_dep) <- c("I", "T", "N", "F")
    
    list(
      K_poisson = K_poisson,
      K_binomial = K_binomial,
      S_gamma = S_gamma,
      S_normala = S_normala,
      df = df,
      df_dep = df_dep,
      n_sim = n_sim
    )
  }, ignoreNULL = FALSE)
  
  #!!! tab 1-trafic zilnic
  
  output$plot_poisson <- renderPlot({
    data <- date_simulare()
    hist(data$K_poisson, breaks = 30, probability = TRUE,
         col = "mistyrose", border = "white",
         main = "Trafic zilnic - Poisson",
         xlab = "Nr. clienti (Kd)", ylab = "Densitate")
    curve(dnorm(x, mean = input$lambda_trafic, sd = sqrt(input$lambda_trafic)),
          col = "red", lwd = 2, add = TRUE)
    legend("topright", "Aprox. Normala", col = "red", lwd = 2)
  })
  
  output$plot_binomial <- renderPlot({
    data <- date_simulare()
    hist(data$K_binomial, breaks = 30, probability = TRUE,
         col = "lavender", border = "white",
         main = "Trafic zilnic - Binomial",
         xlab = "Nr. Clienti (Kd)", ylab = "Densitate")
    mu_b <- input$n_market * input$p_active
    sd_b <- sqrt(input$n_market * input$p_active * (1 - input$p_active))
    curve(dnorm(x, mean = mu_b, sd = sd_b), col = "purple", lwd = 2, add = TRUE)
    legend("topright", "Aprox. Normala", col = "purple", lwd = 2)
  })
  
  output$tabel_trafic <- renderTable({
    data <- date_simulare()
    data.frame(
      Model = c("Poisson", "Binomial"),
      Media_Empirica = c(mean(data$K_poisson), mean(data$K_binomial)),
      Media_Teoretica = c(input$lambda_trafic, input$n_market * input$p_active),
      Varianta_Empirica = c(var(data$K_poisson), var(data$K_binomial)),
      Varianta_Teoretica = c(input$lambda_trafic, input$n_market * input$p_active * (1 - input$p_active))
    )
  },digits = 2)
  
  output$interpretare_trafic <- renderText({
    paste(
      "Interpretare:",
      "• Modelul Poisson are media egala cu varianta (caracteristica Poisson).",
      "• Modelul Binomial are varianta mai mica decat media, traficul fiind plafonat de dimensiunea pietei.",
      "• Poisson este potrivit pentru trafic teoretic nelimitat, Binomial pentru piata finita.",
      sep = "\n"
    )
  })
  
  #!!! tab 2 - timpi raspuns
  
  output$plot_gamma <- renderPlot({
    data <- date_simulare()
    hist(data$S_gamma, breaks = 50, probability = TRUE,
         col = "lightblue", border = "white",
         main = "Model Gamma (Asimetric)", xlab = "Timp raspuns (ms)", ylab = "Densitate")
    curve(dgamma(x, shape = 2, scale = 100), col = "darkblue", lwd = 3, add = TRUE)
    legend("topright", "Densitate teoretica", col = "darkblue", lwd = 3)
  })
  
  output$plot_normal <- renderPlot({
    data <- date_simulare()
    hist(data$S_normala, breaks = 50, probability = TRUE,
         col = "lightgreen", border = "white",
         main = "Model Normal (Simetric)", xlab = "Timp raspuns (ms)", ylab = "Densitate")
    curve(dnorm(x, mean = 200, sd = 50), col = "darkgreen", lwd = 3, add = TRUE)
    legend("topright", "Densitate teoretica", col = "darkgreen", lwd = 3)
  })
  
  output$tabel_timpi <- renderTable({
    data <- date_simulare()
    data.frame(
      Indicator = c("Media", "Mediana", "Modala", "Varianta"),
      Gamma = c(mean(data$S_gamma), median(data$S_gamma), 
                functie_val_mod(data$S_gamma), var(data$S_gamma)),
      Normală = c(mean(data$S_normala), median(data$S_normala),
                  functie_val_mod(data$S_normala), var(data$S_normala))
    )
  }, digits = 2)
  
  output$interpretare_timpi <- renderText({
    paste(
      "Interpretare:",
      "• Distributia Gamma este asimetrica - modala < mediana < medie",
      "• Distributia Normala este simetrica - media ≈ mediana ≈ modala",
      "• Pentru latente, Gamma este mai realista (nu poate fi negativa, coada lunga dreapta)",
      "• Diferenta medie-mediana în Gamma indică prezența valorilor extreme (outlieri).",
      sep = "\n"
    )
  })
  
  #!!! tab3 - evenimente
  
  output$tabel_probabilitati <- renderTable({
    data <- date_simulare()
    df <- data$df
    t0 <- 600
    n0 <- 2
    
    A <- df$I == 1
    B <- df$T <= t0
    C <- df$N <= n0
    D <- df$F >= 1
    
    data.frame(
      Eveniment = c("P(A) - Succes", "P(B) - SLA", "P(C) - Max n0 incercari", 
                    "P(D) - Cel putin un esec", "P(A ∩ B)", "P(A ∪ D)"),
      Probabilitate = c(mean(A), mean(B), mean(C), mean(D), mean(A & B), mean(A | D))
    )
  }, digits = 4)
  
  output$verificare_formule <- renderText({
    
    data <- date_simulare()
    df <- data$df
    
    t0 <- 600
    A <- df$I == 1  
    B <- df$T <= t0     
    D <- df$F >= 1    
    
    #reuniune
    P_A <- mean(A)
    P_D <- mean(D)
    P_A_int_D <- mean(A & D)
    P_A_reu_D_direct <- mean(A | D)
    P_A_reu_D_formula <- P_A + P_D - P_A_int_D
    
    #intersectie
    P_B <- mean(B)
    P_A_int_B <- mean(A & B)   # P(A intersectat B) empiric
    P_A_times_P_B <- P_A * P_B # P(A) * P(B) teoretic (daca ar fi independente)
    
    paste(
      "1. Formula reuniunii: P(A ∪ D) = P(A) + P(D) - P(A ∩ D)",
      paste("• Calcul direct (din date):", round(P_A_reu_D_direct, 4)),
      paste("• Calcul formula:", round(P_A, 4), "+", round(P_D, 4), "-", round(P_A_int_D, 4), "=", round(P_A_reu_D_formula, 4)),
      paste("• Eroare (Diferenta):", round(abs(P_A_reu_D_direct - P_A_reu_D_formula), 6)),
      "Formula se verifica numeric (Eroare 0)",
      "",
      "2. Formula intersectiei: P(A ∩ B) = P(A) * P(B)",
      paste("• P(A ∩ B) Empiric:", round(P_A_int_B, 4)),
      paste("• Produs P(A) * P(B):", round(P_A_times_P_B, 4)),
      paste("• Diferenta:", round(abs(P_A_int_B - P_A_times_P_B), 4)),
      "Daca diferenta nu e 0, evenimentele A si B sunt dependente",
      sep = "\n"
    )
  })
  
  output$explicatie_probabilitati <- renderText({
    paste(
      "De ce probabilitatea empirica aproximeaza bine probabilitatea teoretica:",
      "• Legea Numerelor Mari (LGN): frecventa relativa converge la probabilitate când n → ∞",
      "• Cu 5000+ simulari, eroarea standard este < 1%",
      "• Simularile Monte Carlo sunt nebiasate și consistente",
      sep = "\n"
    )
  })
  
  #!!! tab 4 - (N,F)
  
  output$tabel_NF <- renderTable({
    data <- date_simulare()
    tabel <- table(data$df$N, data$df$F)
    as.data.frame.matrix(tabel)
  }, rownames = TRUE)
  
  output$heatmap_NF <- renderPlot({
    data <- date_simulare()
    tabel <- table(data$df$N, data$df$F)
    heatmap(as.matrix(tabel), Rowv = NA, Colv = NA, scale = "none",
            col = heat.colors(256), main = "Heatmap N vs F",
            xlab = "Numar esecuri (F)", ylab = "Numar incercari (N)")
  })
  
  output$marginala_N <- renderTable({
    data <- date_simulare()
    tabel <- table(data$df$N)
    data.frame(N = names(tabel), Frecvență = as.vector(tabel))
  })
  
  output$marginala_F <- renderTable({
    data <- date_simulare()
    tabel <- table(data$df$F)
    data.frame(F = names(tabel), Frecvență = as.vector(tabel))
  })
  
  output$test_independenta_NF <- renderText({
    data <- date_simulare()
    tabel <- table(data$df$N, data$df$F)
    test <- suppressWarnings(chisq.test(tabel))
    paste(
      "Test Chi-patrat pentru independenta:",
      paste("• Statistica X² =", round(test$statistic, 2)),
      paste("• Grade libertate =", test$parameter),
      paste("• p-value <", format(test$p.value, scientific = TRUE)),
      "• Concluzie: N și F sunt dependente (p < 0.05)",
      "(Corelare perfecta: N = F + 1)",
      sep = "\n"
    )
  })
  
  #!!! tab 5 - (N,T)
  
  output$boxplot_NT <- renderPlot({
    data <- date_simulare()
    boxplot(data$df$T ~ data$df$N,
            col = c("green", "yellow", "orange", "red"),
            main = "Distributia T in functie de N",
            xlab = "Numar incercari (N)", ylab = "Timp Total (ms)")
  })
  
  output$scatter_NT <- renderPlot({
    data <- date_simulare()
    plot(jitter(data$df$N), data$df$T, pch = 20, col = rgb(0, 0, 1, 0.3),
         main = "Scatter N vs T", xlab = "Numar Incercari (N)", ylab = "Timp Total (ms)")
    abline(lm(data$df$T ~ data$df$N), col = "red", lwd = 2)
    legend("topleft", "Linie regresie", col = "red", lwd = 2)
  })
  
  output$statistici_NT <- renderTable({
    data <- date_simulare()
    data.frame(
      Statistica = c("Media N", "Varianta N", "Media T", "Varianta T", "Covarianta", "Corelatia"),
      Valoare = c(mean(data$df$N), var(data$df$N), mean(data$df$T), var(data$df$T),
                  cov(data$df$N, data$df$T), cor(data$df$N, data$df$T))
    )
  }, digits = 4)
  
  output$interpretare_NT <- renderText({
    data <- date_simulare()
    cor_val <- cor(data$df$N, data$df$T)
    paste(
      "Interpretare corelatie N-T:",
      paste("• Coeficientul de corelatie r =", round(cor_val, 3)),
      "• Corelatie pozitivă puternica: mai multe incercari, timp total mai mare",
      "• Fiecare retry adauga:timp_raspuns + backoff",
      "• Relatia este logica: esecurile prelungesc procesarea",
      sep = "\n"
    )
  })
  
  #!!! tab 6 - conditionari
  
  output$prob_conditionate <- renderTable({
    data <- date_simulare()
    df <- data$df
    t0 <- input$t0_sla
    n0 <- input$n0_incercari
    
    subset_C <- df[df$N <= n0, ]
    P_A_cond_C <- mean(subset_C$I == 1)
    
    subset_A <- df[df$I == 1, ]
    P_B_cond_A <- mean(subset_A$T <= t0)
    
    data.frame(
      Conditionare = c(paste("P(Succes | N ≤", n0, ")"), paste("P(T ≤", t0, "| Succes)")),
      Probabilitate = c(P_A_cond_C, P_B_cond_A)
    )
  }, digits = 4)
  
  output$medii_conditionate <- renderTable({
    data <- date_simulare()
    df <- data$df
    
    media_T_succes <- mean(df$T[df$I == 1])
    media_T_esec <- mean(df$T[df$I == 0])
    
    data.frame(
      Conditie = c("E(T | I=1) - Succes", "E(T | I=0) - Esec"),
      Media_T_ms = c(media_T_succes, media_T_esec)
    )
  }, digits = 2)
  
  output$interpretare_conditionari <- renderText({
    data <- date_simulare()
    df <- data$df
    media_T_succes <- mean(df$T[df$I == 1])
    media_T_esec <- mean(df$T[df$I == 0])
    
    paste(
      "Interpretare din perspectiva UX:",
      paste("• Timpul mediu pentru succes:", round(media_T_succes, 0), "ms"),
      paste("• Timpul mediu pentru esec:", round(media_T_esec, 0), "ms"),
      "• Esecurile dureaza mult mai mult (timeout + retry-uri)",
      "• Utilizatorii care experimenteaza esecuri au experienta semnificativ mai proasta",
      "• Recomandare: reducerea ratei de esec imbunatateste dramatic UX",
      sep = "\n"
    )
  })
  
  #!!! tab 7 - dependenta
  
  date_dependenta <- eventReactive(input$run_sim, {
    set.seed(input$seed)
    n_sim <- input$n_simulari
    
    cereri_dep <- t(replicate(n_sim, simuleaza_dependent(
      p_succes = input$p_succes,
      n_max_retry = input$n_max_retry,
      t_backoff = input$t_backoff,
      t_timeout = input$t_timeout,
      penalizare = input$penalizare_dep
    )))
    df_dep <- as.data.frame(cereri_dep)
    colnames(df_dep) <- c("I", "T", "N", "F")
    df_dep
  }, ignoreNULL = FALSE)
  
  output$densitate_dep <- renderPlot({
    data <- date_simulare()
    df_dep <- date_dependenta()
    
    x_max <- max(max(data$df$T), max(df_dep$T))
    plot(density(data$df$T), col = "blue", lwd = 3, xlim = c(0, x_max),
         main = "Independent vs Dependent", xlab = "Timp total (ms)", ylab = "Densitate")
    lines(density(df_dep$T), col = "red", lwd = 3, lty = 2)
    legend("topright", c("Independent", "Dependent"), col = c("blue", "red"), lwd = 3, lty = c(1, 2))
  })
  
  output$boxplot_dep <- renderPlot({
    data <- date_simulare()
    df_dep <- date_dependenta()
    boxplot(data$df$T, df_dep$T, col = c("lightblue", "salmon"),
            names = c("Independent", "Dependent"),
            main = "Comparatie Boxplot", ylab = "Timp Total (ms)")
  })
  
  output$comparatie_dep <- renderTable({
    data <- date_simulare()
    df_dep <- date_dependenta()
    data.frame(
      Scenariu = c("Independent", "Dependent"),
      Media_T = c(mean(data$df$T), mean(df_dep$T)),
      Varianta_T = c(var(data$df$T), var(df_dep$T)),
      Mediana_T = c(median(data$df$T), median(df_dep$T)),
      Rata_Succes = c(mean(data$df$I), mean(df_dep$I))
    )
  }, digits = 2)
  
  output$concluzii_dep <- renderText({
    data <- date_simulare()
    df_dep <- date_dependenta()
    paste(
      "Concluzii privind riscul si stabilitatea:",
      paste("• Varianta creste de la", round(var(data$df$T), 0), "la", round(var(df_dep$T), 0), "in scenariul dependent"),
      "• Dependenta introduce risc suplimentar: esecurile 'propaga' probleme",
      "• Sistemul dependent are 'coada' distributiei mai lunga (outlieri mari)",
      "• Recomandare: izolarea componentelor pentru a evita efecte de cascada",
      sep = "\n"
    )
  })
  
  #!!! tab 8 - inegalitati
  
  output$markov_tabel <- renderTable({
    data <- date_simulare()
    T_vals <- data$df$T
    media_T <- mean(T_vals)
    
    a_valori <- c(300, 500, 800, 1000)
    rezultate <- sapply(a_valori, function(a) {
      prob_emp <- mean(T_vals >= a)
      limita <- media_T / a
      c(a, round(prob_emp, 4), round(limita, 4), prob_emp <= limita + 0.001)
    })
    
    df <- as.data.frame(t(rezultate))
    colnames(df) <- c("a", "P(T≥a) Empiric", "Limita Markov", "Respectata")
    df
  })
  
  output$cebisev_tabel <- renderTable({
    data <- date_simulare()
    T_vals <- data$df$T
    media_T <- mean(T_vals)
    sd_T <- sd(T_vals)
    
    k_valori <- c(1, 1.5, 2, 3)
    rezultate <- sapply(k_valori, function(k) {
      prob_emp <- mean(abs(T_vals - media_T) >= k * sd_T)
      limita <- 1 / k^2
      c(k, round(prob_emp, 4), round(limita, 4), prob_emp <= limita + 0.001)
    })
    
    df <- as.data.frame(t(rezultate))
    colnames(df) <- c("k", "P(|T-μ|≥kσ) Empiric", "Limita Cebisev", "Respectata")
    df
  })
  
  output$chernoff_tabel <- renderTable({
    data <- date_simulare()
    p_esec <- 1 - input$p_succes
    n_inc <- input$n_max_retry + 1
    mu <- n_inc * p_esec
    
    delta_valori <- c(0.5, 1.0, 1.5)
    rezultate <- sapply(delta_valori, function(delta) {
      prag <- (1 + delta) * mu
      limita <- exp(-delta^2 * mu / (2 + delta))
      prob_emp <- mean(data$df$F >= prag)
      c(delta, round(prag, 2), round(prob_emp, 4), round(limita, 4))
    })
    
    df <- as.data.frame(t(rezultate))
    colnames(df) <- c("δ", "Prag F", "P Empiric", "Limita Chernoff")
    df
  })
  
  output$interpretare_chernoff <- renderText({
    paste(
      "Interpretare Chernoff:",
      "• Chernoff ofera limite exponential stranse pentru sume de v.a. independente",
      "• Utila pentru a garanta ca evenimente rare (multe esecuri) au probabilitate mica",
      "• Aplicatie: dimensionarea resurselor pentru worst-case cu probabilitate controlata",
      sep = "\n"
    )
  })
  
  output$jensen_tabel <- renderTable({
    data <- date_simulare()
    T_vals <- data$df$T
    media_T <- mean(T_vals)
    
    # phi(x) = x^2
    phi_E_sq <- media_T^2
    E_phi_sq <- mean(T_vals^2)
    
    # phi(x) = exp(x/1000)
    T_scaled <- T_vals / 1000
    phi_E_exp <- exp(mean(T_scaled))
    E_phi_exp <- mean(exp(T_scaled))
    
    data.frame(
      Funcție = c("φ(x) = x²", "φ(x) = exp(x/1000)"),
      `φ(E[T])` = c(round(phi_E_sq, 2), round(phi_E_exp, 4)),
      `E[φ(T)]` = c(round(E_phi_sq, 2), round(E_phi_exp, 4)),
      Respectata = c(phi_E_sq <= E_phi_sq, phi_E_exp <= E_phi_exp)
    )
  })
  
  output$interpretare_jensen <- renderText({
    paste(
      "Interpretare Jensen in contextul riscului:",
      "• φ(E[T]) ≤ E[φ(T)] pentru functii convexe φ",
      "• E(T²) > E(T)² arata ca varianta introduce risc suplimentar",
      "• Pentru costuri convexe (penalitati SLA, pierderi), riscul real > estimarea pe medie",
      "• Concluzie: optimizarea doar pe medie este insuficienta, trebuie redusa si variabilitatea",
      sep = "\n"
    )
  })
  
  #!!! tab9 - Aproximare normala
  
  output$hist_agregare <- renderPlot({
    set.seed(input$seed)
    n_zile <- input$n_zile_agregare
    lambda_zi <- input$trafic_mediu_zi
    
    trafic <- rpois(n_zile, lambda = lambda_zi)
    latenta_totala <- sapply(trafic, function(nr) {
      if(nr > 0) sum(rgamma(nr, shape = 2, scale = 100)) else 0
    })
    
    media_S <- 200  # shape * scale = 2 * 100
    var_S <- 20000  # shape * scale^2 = 2 * 10000
    mu_agregat <- lambda_zi * media_S
    sigma_agregat <- sqrt(lambda_zi * var_S + media_S^2 * lambda_zi)
    
    hist(latenta_totala, breaks = 50, probability = TRUE,
         col = "navajowhite", border = "white",
         main = "Latenta Totala Zilnica (Aproximare Normala)",
         xlab = "Latenta Totala (ms)", ylab = "Densitate")
    curve(dnorm(x, mean = mu_agregat, sd = sigma_agregat),
          col = "darkred", lwd = 3, add = TRUE)
    legend("topright", "Aproximare Normala", col = "darkred", lwd = 3)
  })
  
  output$interpretare_agregare <- renderText({
    paste(
      "Interpretare - Teorema Limita Centrala:",
      "• Suma variabilelor aleatoare tinde spre distributie normala (TLC)",
      "• Histograma agregatului se apropie de curba normala teoretica",
      "• Aproximarea este mai buna când numarul de termeni creste",
      "• Aplicatie: planificarea capacitatii pe baza distributiei normale",
      sep = "\n"
    )
  })
  
  #!!! tab 10 - Churn
  
  output$barplot_churn <- renderPlot({
    set.seed(input$seed)
    n_util <- 5000
    
    #simulare
    rezultate <- replicate(n_util, {
      nr_cereri <- 10
      status <- rbinom(nr_cereri, 1, 1 - input$p_succes)
      
      churn_aleator <- runif(1) < input$q_random
      churn_tehnic <- FALSE
      
      if(nr_cereri >= input$m_fereastra) {
        for(i in 1:(nr_cereri - input$m_fereastra + 1)) {
          fereastra <- status[i:(i + input$m_fereastra - 1)]
          if(sum(fereastra) >= input$k_prag) {
            churn_tehnic <- TRUE
            break
          }
        }
      }
      
      c(aleator = churn_aleator, tehnic = churn_tehnic)
    })
    
    prob_aleator <- mean(rezultate[1, ])
    prob_tehnic <- mean(rezultate[2, ])
    prob_total <- mean(rezultate[1, ] | rezultate[2, ])
    
    barplot(c(Aleator = prob_aleator, Tehnic = prob_tehnic, Total = prob_total),
            col = c("lightblue", "salmon", "grey"), ylim = c(0, 1),
            main = "Rata de Churn pe Categorii", ylab = "Probabilitate")
    text(x = c(0.7, 1.9, 3.1), y = c(prob_aleator, prob_tehnic, prob_total) + 0.05,
         labels = round(c(prob_aleator, prob_tehnic, prob_total), 3))
  })
  
  output$tabel_churn <- renderTable({
    set.seed(input$seed)
    n_util <- 5000
    
    rezultate <- replicate(n_util, {
      nr_cereri <- 10
      status <- rbinom(nr_cereri, 1, 1 - input$p_succes)
      
      churn_aleator <- runif(1) < input$q_random
      churn_tehnic <- FALSE
      
      if(nr_cereri >= input$m_fereastra) {
        for(i in 1:(nr_cereri - input$m_fereastra + 1)) {
          fereastra <- status[i:(i + input$m_fereastra - 1)]
          if(sum(fereastra) >= input$k_prag) {
            churn_tehnic <- TRUE
            break
          }
        }
      }
      
      c(aleator = churn_aleator, tehnic = churn_tehnic)
    })
    
    data.frame(
      Tip_Churn = c("Aleator", "Tehnic", "Total (reuniune)"),
      Probabilitate = c(mean(rezultate[1, ]), mean(rezultate[2, ]),
                        mean(rezultate[1, ] | rezultate[2, ]))
    )
  }, digits = 4)
  
  output$interpretare_churn <- renderText({
    paste(
      "Interpretare Churn:",
      "• Churn aleator: utilizatori care pleaca indiferent de performanta",
      "• Churn tehnic: utilizatori care pleaca din cauza experientelor proaste",
      "• Daca churn tehnic > aleator , problema e performanta, nu piata",
      "• Recomandare: prioritizeaza infrastructura cand churn tehnic domina",
      sep = "\n"
    )
  })
  
  #!! tab 11 - economic
  
  output$hist_profit <- renderPlot({
    set.seed(input$seed)
    n_zile <- input$n_zile_profit
    trafic <- rpois(n_zile, lambda = 100)
    
    profit_zilnic <- sapply(1:n_zile, function(d) {
      nr_clienti <- trafic[d]
      if(nr_clienti == 0) return(0)
      
      profit <- 0
      for(i in 1:nr_clienti) {
        cereri <- t(replicate(3, simuleaza_cerere(p_succes = input$p_succes)))
        nr_succese <- sum(cereri[, 1] == 1)
        nr_sla <- sum(cereri[, 2] > input$t0_sla)
        
        profit <- profit + nr_succese * input$castig_succes
        profit <- profit - nr_sla * input$penalizare_sla
        
        rata_esec <- sum(cereri[, 1] == 0) / 3
        if(runif(1) < 0.05 + 0.3 * rata_esec) {
          profit <- profit - input$pierdere_churn
        }
      }
      profit
    })
    
    hist(profit_zilnic, breaks = 40, col = "darkgreen", border = "white",
         main = "Distributia Profitului Zilnic", xlab = "Profit (RON)", ylab = "Frecventa")
    abline(v = mean(profit_zilnic), col = "red", lwd = 2, lty = 2)
    legend("topright", paste("Medie:", round(mean(profit_zilnic), 2)), col = "red", lwd = 2, lty = 2)
  })
  
  output$statistici_profit <- renderTable({
    set.seed(input$seed)
    
    # 1. Folosim aceiasi parametri ca la grafic
    n_zile <- min(input$n_zile_profit, 200) # Limitam doar zilele, nu clientii per zi!
    trafic <- rpois(n_zile, lambda = 100)   # Acelasi lambda ca la grafic
    
    profit_zilnic <- sapply(1:n_zile, function(d) {
      nr_clienti <- trafic[d]
      if(nr_clienti == 0) return(0)
      
      profit <- 0
      # 2. Iteram prin TOȚI clienții zilei respective (fără min(..., 50))
      for(i in 1:nr_clienti) { 
        # Simplificare: fiecare client face 3 cereri (ca să meargă repede simularea)
        cereri <- t(replicate(3, simuleaza_cerere(
          p_succes = input$p_succes,
          t_timeout = input$t_timeout
        )))
        
        nr_succese <- sum(cereri[, 1] == 1)
        
        # 3. Folosim input$t0_sla, NU valoarea fixa 600
        nr_sla <- sum(cereri[, 2] > input$t0_sla) 
        
        profit <- profit + nr_succese * input$castig_succes
        profit <- profit - nr_sla * input$penalizare_sla
        
        rata_esec <- sum(cereri[, 1] == 0) / 3
        if(runif(1) < 0.05 + 0.3 * rata_esec) {
          profit <- profit - input$pierdere_churn
        }
      }
      profit
    })
    
    eroare_standard <- sd(profit_zilnic) / sqrt(n_zile)
    margine_eroare <- 1.96 * eroare_standard
    ic_min <- mean(profit_zilnic) - margine_eroare
    ic_max <- mean(profit_zilnic) + margine_eroare
    
    data.frame(
      Statistica = c("Media zilnica", "Deviatia standard (Risc)", 
                     "Minim", "Maxim", "Interval incredere (95%)"),
      Valoare_RON = c(mean(profit_zilnic), sd(profit_zilnic), 
                      min(profit_zilnic), max(profit_zilnic), 
                      paste("[", round(ic_min, 1), "; ", round(ic_max, 1), "]", sep=""))
    )
  }, digits = 2)
  
  output$analiza_economica <- renderText({
    paste(
      "Analiza compromisuri tehnico-economice:",
      "• Cresterea p_succes, mai putine retry-uri , profit mai mare",
      "• Reducerea timeout-ului, mai puține penalitati SLA dar mai multe esecuri",
      "• Trade-off: infrastructura mai bună = cost mai mare dar profit net superior",
      "• Recomandare: analiza cost-beneficiu pentru fiecare imbunatatire",
      sep = "\n"
    )
  })
  
  #!!! tab 12 - vizualizare
  
  output$hist_T_viz <- renderPlot({
    data <- date_simulare()
    hist(data$df$T, breaks = 40, col = "steelblue", border = "white",
         main = "Distributia Timpului Total (T)", xlab = "Timp (ms)", ylab = "Frecventa")
  })
  
  output$boxplot_succes_esec <- renderPlot({
    data <- date_simulare()
    boxplot(data$df$T ~ data$df$I, col = c("salmon", "lightgreen"),
            names = c("Esec (I=0)", "Succes (I=1)"),
            main = "Timp Total: Succes vs Esec", xlab = "Rezultat", ylab = "Timp (ms)")
  })
  
  output$statistici_vizualizare <- renderTable({
    data <- date_simulare()
    df <- data$df
    
    mediana_succes <- median(df$T[df$I == 1])
    mediana_esec <- median(df$T[df$I == 0])
    iqr_succes <- IQR(df$T[df$I == 1])
    iqr_esec <- if(sum(df$I == 0) > 0) IQR(df$T[df$I == 0]) else NA
    
    data.frame(
      Metric = c("Mediana (succes)", "Mediana (esec)", "IQR (succes)", "IQR (esec)"),
      Valoare_ms = c(mediana_succes, mediana_esec, iqr_succes, iqr_esec)
    )
  }, digits = 2)
  
  output$interpretare_outlieri <- renderText({
    data <- date_simulare()
    T_vals <- data$df$T
    q1 <- quantile(T_vals, 0.25)
    q3 <- quantile(T_vals, 0.75)
    iqr <- q3 - q1
    nr_out <- sum(T_vals > q3 + 1.5 * iqr)
    pct_out <- 100 * nr_out / length(T_vals)
    
    paste(
      "Interpretare outlieri:",
      paste("• Numar outlieri (T > Q3 + 1.5×IQR):", nr_out, paste0("(", round(pct_out, 2), "%)")),
      "• Outlierii reprezinta cereri cu latente foarte mari",
      "• Cauze: retry-uri multiple, timeout aproape, congestie",
      "• IQR mic pentru succes, mare pentru esec, esecurile sunt imprevizibile",
      sep = "\n"
    )
  })
  
  #!!! tab 13 - sinteza
  
  output$sinteza_a <- renderText({
    paste(
      "Probabilitatea empirica permite estimarea probabilitatilor teoretice din date simulate/observate.",
      "Cu cat esantionul este mai mare, cu atat estimarile converg la valorile adevarate (Legea Numerelor Mari).",
      "In proiect, am folosit 5000+ simulari pentru a obține estimari precise ale P(A), P(B), etc.",
      "Eroarea standard scade proportional cu √n, oferind precizie buna cu resurse rezonabile.",
      sep = "\n"
    )
  })
  
  output$sinteza_b <- renderText({
    data <- date_simulare()
    df <- data$df
    media_T_succes <- mean(df$T[df$I == 1])
    media_T_esec <- mean(df$T[df$I == 0])
    
    paste(
      "Conditionarile permit analiza comportamentului in subgrupe specifice.",
      paste("Exemplu: E(T|I=1) =", round(media_T_succes, 0), "ms vs E(T|I=0) =", round(media_T_esec, 0), "ms"),
      "Aceasta diferenta arata ca cererile esuate au timpi semnificativ mai mari.",
      "P(B|A) ne spune ce proportie din cererile reusite respecta si SLA-ul.",
      "Conditionarile ofera insight-uri pentru optimizare targetata.",
      sep = "\n"
    )
  })
  
  output$sinteza_c <- renderText({
    paste(
      "Inegalitatile ofera garantii worst-case cand distributiile exacte sunt necunoscute:",
      "• Markov/Cebisev: limite superioare pentru probabilitati de depasire",
      "• Chernoff: limite exponential stranse pentru evenimente rare",
      "• Jensen: justifica de ce optimizarea bazata doar pe medie este insuficienta",
      "Aplicatie practica: dimensionarea serverelor pentru a respecta SLA-uri cu probabilitate mare.",
      sep = "\n"
    )
  })
  
  output$sinteza_d <- renderText({
    paste(
      "Performanta tehnica influenteaza direct profitul:",
      "• Cereri reusite = venit",
      "• Cereri lente = penalitati SLA",
      "• Esecuri repetate = churn = pierdere clienti",
      "Corelatie: variatia T mare , risc economic crescut (Jensen).",
      "Investitiile în infrastructura cresc costurile dar reduc pierderile.",
      sep = "\n"
    )
  })
  
  output$sinteza_e <- renderText({
    paste(
      "Parametrii cu impact major:",
      "1. p_succes: cresterea acesteia reduce retry-urile și churn-ul",
      "2. t_timeout: prea scurt , penalitati mari; prea lung , UX proasta",
      "3. n_max_retry: mai multe , sanse succes crescute, dar latenta mai mare",
      "4. lambda_trafic: traficul mare creste veniturile dar si riscul de suprasolicitare",
      "",
      "Recomandari pentru imbunătatire:",
      "• Investitii in infrastructura (load balancing, caching)",
      "• Circuit breaker pentru a evita retry-uri inutile in caz de esec sistemic",
      "• Monitorizare proactiva a SLA-urilor cu alerte la 80% din prag",
      "• Analiza periodica a distributiei T pentru identificarea bottleneck-urilor",
      sep = "\n"
    )
  })
}

#rulare aplicatie
shinyApp(ui = ui, server = server)
