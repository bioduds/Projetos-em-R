library(shiny)
library(ggplot2)
library(httr2)

env_file <- file.path(getwd(), ".env")
if (file.exists(env_file) && !readRenviron(env_file)) {
  stop("Não foi possível carregar o arquivo .env.", call. = FALSE)
}

difficulty_labels <- c(
  impossible = "Impossível",
  nearly_impossible = "Quase impossível",
  extremely_hard = "Extremamente difícil",
  very_hard = "Muito difícil",
  hard = "Difícil",
  challenging = "Desafiador",
  medium = "Médio",
  moderately_easy = "Moderadamente fácil",
  easy = "Fácil",
  very_easy = "Muito fácil",
  almost_trivial = "Quase trivial",
  trivial = "Trivial"
)

difficulty_mu <- c(
  impossible = 97,
  nearly_impossible = 93,
  extremely_hard = 89,
  very_hard = 85,
  hard = 70,
  challenging = 60,
  medium = 50,
  moderately_easy = 40,
  easy = 30,
  very_easy = 15,
  almost_trivial = 8,
  trivial = 3
)

variance_labels <- c(
  chaotic = "Caótica",
  erratic = "Errática",
  medium = "Média",
  expected = "Esperada",
  predictable = "Previsível"
)

variance_sigma <- c(
  chaotic = 20,
  erratic = 15,
  medium = 10,
  expected = 5,
  predictable = 1
)

sigma_band_labels <- c(
  "< -5σ | < 0,00003%",
  "-5 a -4σ | 0,00003–0,00317%",
  "-4 a -3σ | 0,00317–0,13499%",
  "-3 a -2σ | 0,13499–2,27501%",
  "-2 a -1σ | 2,27501–15,8655%",
  "-1 a 0σ | 15,8655–50%",
  "0 a +1σ | 50–84,1345%",
  "+1 a +2σ | 84,1345–97,725%",
  "+2 a +3σ | 97,725–99,865%",
  "+3 a +4σ | 99,865–99,9968%",
  "+4 a +5σ | 99,9968–99,99997%",
  "> +5σ | > 99,99997%"
)

sigma_adjectives <- c(
  "Inimaginável",
  "Inacreditável",
  "Raríssimo",
  "Escasso",
  "Incomum",
  "Corriqueiro",
  "Habitual",
  "Atípico",
  "Excecional",
  "Extraordinário",
  "Fenomenal",
  "Inconcebível"
)

sigma_adjective_for_score <- function(sigma_score) {
  if (length(sigma_score) != 1L || !is.numeric(sigma_score) ||
      !is.finite(sigma_score)) {
    stop("O desvio padronizado deve ser um valor finito.", call. = FALSE)
  }

  band_index <- findInterval(sigma_score, -5:5) + 1L
  sigma_adjectives[[band_index]]
}

sigma_band_for_score <- function(sigma_score) {
  if (length(sigma_score) != 1L || !is.numeric(sigma_score) ||
      !is.finite(sigma_score)) {
    stop("O desvio padronizado deve ser um valor finito.", call. = FALSE)
  }

  band_index <- findInterval(sigma_score, -5:5) + 1L
  sigma_band_labels[[band_index]]
}

truncated_normal_bounds <- function(mu, sigma) {
  lower <- stats::pnorm(0, mean = mu, sd = sigma)
  upper <- stats::pnorm(100, mean = mu, sd = sigma)
  span <- upper - lower
  if (!is.finite(span) || span <= 0) {
    stop("Não foi possível calcular a distribuição limitada do d100.",
         call. = FALSE)
  }

  c(lower = lower, span = span)
}

smoothstep <- function(value) {
  value <- pmin(pmax(value, 0), 1)
  value^2 * (3 - 2 * value)
}

inverse_smoothstep <- function(value) {
  value <- pmin(pmax(value, 0), 1)
  0.5 - sin(asin(1 - 2 * value) / 3)
}

d100_percentile <- function(roll, mu, sigma) {
  bounds <- truncated_normal_bounds(mu, sigma)
  normal_percentile <- (
    stats::pnorm(roll, mean = mu, sd = sigma) - bounds[["lower"]]
  ) / bounds[["span"]]
  smoothstep(normal_percentile)
}

d100_roll_for_percentile <- function(percentile, mu, sigma) {
  bounds <- truncated_normal_bounds(mu, sigma)
  normal_percentile <- inverse_smoothstep(percentile)
  roll <- stats::qnorm(
    bounds[["lower"]] + normal_percentile * bounds[["span"]],
    mean = mu,
    sd = sigma
  )
  pmin(pmax(roll, 0), 100)
}

sample_action <- function(mu, sigma) {
  if (length(mu) != 1L || !is.numeric(mu) || !is.finite(mu) ||
      mu < 0 || mu > 100 ||
      length(sigma) != 1L || !is.numeric(sigma) || !is.finite(sigma) ||
      sigma <= 0) {
    stop("A dificuldade deve estar entre 0 e 100 e a variabilidade deve ser positiva.",
         call. = FALSE)
  }

  percentile <- stats::runif(1)
  roll <- d100_roll_for_percentile(percentile, mu, sigma)
  sigma_score <- (roll - mu) / sigma

  list(
    roll = roll,
    percentile = percentile,
    expected_roll = mu,
    roll_delta = roll - mu,
    sigma_score = sigma_score,
    normal_percentile = stats::pnorm(sigma_score),
    sigma_band = sigma_band_for_score(sigma_score),
    sigma_adjective = sigma_adjective_for_score(sigma_score)
  )
}

parse_choice_answer <- function(answer, valid_options, answer_name) {
  if (!is.list(answer) || !identical(answer$type, "choice") ||
      !is.character(answer$choice) || length(answer$choice) != 1L ||
      is.na(answer$choice) || !answer$choice %in% valid_options) {
    stop(sprintf("Resposta inválida do Jev para %s.", answer_name), call. = FALSE)
  }

  confidence <- answer$confidence
  probabilities <- answer$probabilities
  if (!is.numeric(confidence) || length(confidence) != 1L ||
      !is.finite(confidence) || confidence < 0 || confidence > 1 ||
      !is.list(probabilities) ||
      !setequal(names(probabilities), valid_options)) {
    stop(sprintf("Confiança ou probabilidades inválidas do Jev para %s.", answer_name),
         call. = FALSE)
  }

  probabilities <- unlist(probabilities, use.names = TRUE)
  if (!is.numeric(probabilities) || any(!is.finite(probabilities)) ||
      any(probabilities < 0 | probabilities > 1) ||
      abs(sum(probabilities) - 1) > 0.01) {
    stop(sprintf("Distribuição de probabilidades inválida do Jev para %s.", answer_name),
         call. = FALSE)
  }

  list(
    choice = answer$choice,
    confidence = confidence,
    probabilities = probabilities
  )
}

classify_event_with_jev <- function(event_description) {
  api_key <- trimws(Sys.getenv("TYPESAFE_API_KEY", unset = ""))
  if (!nzchar(api_key)) {
    stop("Configure a variável de ambiente TYPESAFE_API_KEY para usar o Jev.",
         call. = FALSE)
  }

  difficulty_criteria <- list(
    impossible = "Impossível; dificuldade-base 97. Só seria alcançável por uma mudança radical nas condições.",
    nearly_impossible = "Quase impossível; dificuldade-base 93. Exige circunstâncias extraordinárias e uma execução excepcional.",
    extremely_hard = "Extremamente difícil; dificuldade-base 89. Exige preparo excepcional e superar grandes obstáculos.",
    very_hard = "Muito difícil; dificuldade-base 85. Exige habilidade elevada e superar obstáculos importantes.",
    hard = "Difícil; dificuldade-base 70. Exige esforço, competência ou estratégia consideráveis.",
    challenging = "Desafiador; dificuldade-base 60. Exige atenção e habilidade, com obstáculos relevantes, mas manejáveis.",
    medium = "Médio; dificuldade-base 50. Um desafio equilibrado, compatível com a capacidade usual do personagem.",
    moderately_easy = "Moderadamente fácil; dificuldade-base 40. A capacidade do personagem tende a superar o desafio.",
    easy = "Fácil; dificuldade-base 30. Pouco esforço ou habilidade específica são necessários.",
    very_easy = "Muito fácil; dificuldade-base 15. Quase não há obstáculos relevantes para o personagem.",
    almost_trivial = "Quase trivial; dificuldade-base 8. A ação é simples e requer atenção mínima.",
    trivial = "Trivial; dificuldade-base 3. A ação é praticamente imediata para o personagem."
  )
  variance_criteria <- list(
    chaotic = paste(
      "Muito alta imprevisibilidade; desvio padrão-base 20.",
      "As circunstâncias mudam sem padrão e podem levar a resultados muito diferentes."
    ),
    erratic = paste(
      "Alta imprevisibilidade; desvio padrão-base 15.",
      "Há fatores instáveis que podem alterar bastante o resultado, mas alguns limites são conhecidos."
    ),
    medium = paste(
      "Imprevisibilidade moderada; desvio padrão-base 10.",
      "O resultado é parcialmente previsível, com algumas circunstâncias capazes de desviá-lo."
    ),
    expected = paste(
      "Baixa imprevisibilidade; desvio padrão-base 5.",
      "O resultado provável é conhecido e poucas variações pequenas são esperadas."
    ),
    predictable = paste(
      "Imprevisibilidade mínima; desvio padrão-base 1.",
      "O resultado é praticamente determinado pelas condições conhecidas e varia muito pouco."
    )
  )

  request_body <- list(
    model = "jev-latest",
    state = event_description,
    questions = list(
      difficulty = list(
        type = "choice",
        instructions = paste(
          "Classifique a dificuldade da ação descrita.",
          "Escolha a opção que melhor representa o desafio para o personagem."
        ),
        criteria = difficulty_criteria
      ),
      variance = list(
        type = "choice",
        instructions = paste(
          "Classifique exclusivamente a imprevisibilidade do resultado desta ação, não a dificuldade de realizá-la.",
          "Pergunte: mantendo a ação e a competência do personagem, quão incerto é o resultado por causa de acaso, fatores ocultos ou circunstâncias instáveis?",
          "Se o resultado pode ser previsto com segurança e deve variar pouco, escolha baixa variância (Esperada ou Previsível).",
          "Se vários fatores instáveis podem mudar o resultado de forma significativa, escolha alta variância (Errática ou Caótica).",
          "Uma tarefa difícil pode ter baixa variância quando o resultado é previsível; uma tarefa fácil pode ter alta variância quando há muito acaso.",
          "Não use a chance de sucesso, a complexidade ou o esforço como substitutos da imprevisibilidade."
        ),
        criteria = variance_criteria
      )
    )
  )

  response <- httr2::request("https://api.typesafe.ai/v1/systemone") |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_json(request_body) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  httr2::resp_check_status(response)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)

  if (!is.list(body$answers) ||
      is.null(body$answers$difficulty) ||
      is.null(body$answers$variance)) {
    stop("A resposta do Jev não contém as duas classificações esperadas.",
         call. = FALSE)
  }

  list(
    difficulty = parse_choice_answer(
      body$answers$difficulty,
      names(difficulty_mu),
      "dificuldade"
    ),
    variance = parse_choice_answer(
      body$answers$variance,
      names(variance_sigma),
      "variância"
    )
  )
}

action_narration_facts <- function(result) {
  paste(
    "Fatos da ação:",
    "",
    paste("Evento:", result$event),
    paste("Dificuldade:", result$difficulty),
    paste("Rolagem esperada (μ):", format(round(result$expected_roll, 2), nsmall = 2)),
    paste("Variância:", result$variance),
    paste("Desvio padrão da variância (σ):", format(round(result$sigma, 2), nsmall = 2)),
    paste("Valor rolado:", format(round(result$roll, 2), nsmall = 2)),
    paste("Diferença em relação ao esperado:", format(round(result$roll_delta, 2), nsmall = 2)),
    paste("Percentil acumulado do modelo d100:", format(round(result$percentile * 100, 2), nsmall = 2), "%"),
    paste("Desvio padronizado em relação ao esperado:", format(round(result$sigma_score, 2), nsmall = 2), "σ"),
    paste("Percentil normal de referência:", format(round(result$normal_percentile * 100, 2), nsmall = 2), "%"),
    paste("Faixa sigma:", result$sigma_band, "—", result$sigma_adjective),
    sep = "\n"
  )
}

narrate_action_with_ollama <- function(result) {
  response <- httr2::request("http://127.0.0.1:11434/api/chat") |>
    httr2::req_body_json(list(
      model = "gemma4:latest",
      messages = list(
        list(
          role = "system",
          content = paste(
            "Você é a narradora de um RPG de mesa.",
            "Crie, em português brasileiro, uma narração concisa e envolvente baseada nos fatos fornecidos.",
            "A rolagem esperada (μ) é o centro de referência: compare o valor rolado com μ e considere a diferença e o desvio padronizado (z = (rolagem − μ) / σ).",
            "Uma diferença próxima de zero é um resultado comum para aquela ação; diferença negativa indica resultado abaixo do esperado, e positiva indica acima. Quanto maior o valor absoluto de z, mais incomum é o resultado. Use o adjetivo sigma correspondente para expressar essa intensidade.",
            "O percentil acumulado do modelo d100 vem da curva suavizada; o percentil normal de referência e o z-score são apresentados separadamente. Use z-score e adjetivo para interpretar a distância em relação ao esperado, sem tratar o percentil suavizado como sigma.",
            "A dificuldade descreve o desafio e a variância descreve a imprevisibilidade. Considere ambas ao narrar o efeito da rolagem: em uma ação difícil, um resultado perto da média continua sendo esperado para esse desafio; uma rolagem acima ou abaixo da média deve alterar proporcionalmente o tom ou o grau de progresso.",
            "Calibração: com μ=70 e σ=10, uma rolagem 54,86 fica 15,14 abaixo do esperado (z≈−1,51σ, Incomum): narre uma complicação moderada, não uma catástrofe. Uma rolagem 77,45 fica 7,45 acima (z≈+0,75σ, Habitual): é um resultado favorável, mas ainda dentro do padrão comum. Uma rolagem 69,78 fica praticamente na média (z≈−0,02σ, Corriqueiro): narre o curso esperado da ação, sem exagerar sucesso ou revés.",
            "Não imponha uma conclusão binária de sucesso ou fracasso. Narre o que aconteceu e o grau de progresso, complicação ou vantagem de forma proporcional ao evento e à posição do resultado em relação ao esperado.",
            "Use explicitamente o adjetivo informado e não contradiga a descrição do evento nem os dados da rolagem."
          )
        ),
        list(
          role = "user",
          content = action_narration_facts(result)
        )
      ),
      stream = FALSE
    )) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()
  httr2::resp_check_status(response)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  narration <- body$message$content

  if (!is.character(narration) || length(narration) != 1L ||
      is.na(narration) || !nzchar(trimws(narration))) {
    stop("Ollama retornou uma narração vazia ou em formato inválido.",
         call. = FALSE)
  }

  narration
}

ui <- fluidPage(
  title = "Sala de Controle do DM",
  fluidRow(
    wellPanel(
      style = "background-color:#494949;",
      width = "100%",
      h1(
        "Sala de Controle do DM",
        style = "text-align:center;font-weight:bold;color:white;"
      )
    )
  ),
  fluidRow(
    column(
      width = 4,
      h2("Descreva o evento"),
      textAreaInput(
        "eventDescription",
        "O que o personagem pretende fazer?",
        value = "",
        rows = 4,
        width = "100%",
        placeholder = "Ex.: atravessar uma ponte antiga enquanto ela desaba..."
      ),
      actionButton("analyzeEvent", "Analisar evento com Jev"),
      hr(),
      uiOutput("statusText"),
      uiOutput("jevSummary"),
      hr(),
      h3("Dificuldade e variância"),
      selectInput(
        "selectDifficulty",
        "Seleção de dificuldade",
        choices = setNames(names(difficulty_labels), unname(difficulty_labels)),
        selected = "medium"
      ),
      numericInput(
        "mu",
        "Dificuldade (0–100; menor = mais fácil)",
        value = 50,
        min = 0,
        max = 100
      ),
      selectInput(
        "selectVariance",
        "Variância do efeito",
        choices = setNames(names(variance_labels), unname(variance_labels)),
        selected = "medium"
      ),
      numericInput(
        "sigma",
        "Variabilidade (desvio padrão σ)",
        value = 10,
        min = 0.1,
        step = 0.1
      ),
      actionButton("rollAction", "Rodar ação", class = "btn-primary"),
      hr()
    ),
    column(
      width = 8,
      h3("Distribuição dos resultados"),
      plotOutput("plotDistribution", height = "590px"),
      hr(),
      h4("Resultado da ação"),
      uiOutput("actionResult"),
      h4("Narração"),
      uiOutput("narration")
    )
  )
)

server <- function(input, output, session) {
  jev_result <- reactiveVal(NULL)
  action_result <- reactiveVal(NULL)
  narration <- reactiveVal(NULL)
  status_message <- reactiveVal("Descreva o evento e peça uma análise ao Jev.")

  observeEvent(input$eventDescription, ignoreInit = TRUE, {
    analysis <- jev_result()
    if (!is.null(analysis) &&
        !identical(trimws(input$eventDescription), analysis$event)) {
      jev_result(NULL)
      action_result(NULL)
      narration(NULL)
      status_message("A descrição mudou. Analise o evento novamente antes de rolar.")
    }
  })

  observeEvent(input$analyzeEvent, {
    event <- trimws(input$eventDescription)
    action_result(NULL)
    narration(NULL)
    jev_result(NULL)

    if (!nzchar(event)) {
      status_message("Descreva o evento antes de solicitar a análise.")
      return()
    }

    status_message("Consultando o Jev...")
    result <- tryCatch(
      classify_event_with_jev(event),
      error = function(error) {
        status_message(paste("Não foi possível classificar o evento:", conditionMessage(error)))
        NULL
      }
    )
    if (is.null(result)) {
      return()
    }

    jev_result(list(event = event, classification = result))
    updateSelectInput(session, "selectDifficulty", selected = result$difficulty$choice)
    updateNumericInput(
      session,
      "mu",
      value = unname(difficulty_mu[[result$difficulty$choice]])
    )
    updateSelectInput(session, "selectVariance", selected = result$variance$choice)
    updateNumericInput(
      session,
      "sigma",
      value = unname(variance_sigma[[result$variance$choice]])
    )
    status_message("Análise concluída. Revise ou ajuste os parâmetros antes de rodar.")
  })

  observeEvent(input$selectDifficulty, ignoreInit = TRUE, {
    value <- difficulty_mu[[input$selectDifficulty]]
    if (!is.null(value)) {
      updateNumericInput(session, "mu", value = unname(value))
    }
  })

  observeEvent(input$selectVariance, ignoreInit = TRUE, {
    value <- variance_sigma[[input$selectVariance]]
    if (!is.null(value)) {
      updateNumericInput(session, "sigma", value = unname(value))
    }
  })

  observeEvent(list(input$mu, input$sigma), ignoreInit = TRUE, {
    action_result(NULL)
    narration(NULL)
  })

  observeEvent(input$rollAction, {
    analysis <- jev_result()
    if (is.null(analysis)) {
      status_message("Analise o evento com Jev antes de rodar a ação.")
      return()
    }
    if (!identical(trimws(input$eventDescription), analysis$event)) {
      jev_result(NULL)
      status_message("A descrição mudou. Analise o evento novamente antes de rolar.")
      return()
    }

    mu <- suppressWarnings(as.numeric(input$mu))
    sigma <- suppressWarnings(as.numeric(input$sigma))
    if (length(mu) != 1L || !is.finite(mu) || mu < 0 || mu > 100 ||
        length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
      status_message("Informe dificuldade entre 0 e 100 e variabilidade maior que zero.")
      return()
    }

    action <- sample_action(mu, sigma)
    result <- list(
      event = analysis$event,
      difficulty = unname(difficulty_labels[[input$selectDifficulty]]),
      variance = unname(variance_labels[[input$selectVariance]]),
      mu = mu,
      sigma = sigma,
      roll = action$roll,
      percentile = action$percentile,
      expected_roll = action$expected_roll,
      roll_delta = action$roll_delta,
      sigma_score = action$sigma_score,
      normal_percentile = action$normal_percentile,
      sigma_band = action$sigma_band,
      sigma_adjective = action$sigma_adjective
    )
    action_result(result)
    narration(NULL)
    status_message("Ação calculada. Gerando a narração local...")

    generated_narration <- tryCatch(
      narrate_action_with_ollama(result),
      error = function(error) {
        status_message(paste("A ação foi calculada, mas a narração falhou:", conditionMessage(error)))
        NULL
      }
    )
    if (!is.null(generated_narration)) {
      narration(generated_narration)
      status_message("Ação e narração concluídas.")
    }
  })

  output$statusText <- renderUI(tags$p(status_message()))

  output$jevSummary <- renderUI({
    analysis <- jev_result()
    if (is.null(analysis)) {
      return(NULL)
    }

    classification <- analysis$classification
    difficulty_probabilities <- classification$difficulty$probabilities
    variance_probabilities <- classification$variance$probabilities
    probability_text <- function(probabilities, labels) {
      option_labels <- unname(labels[names(probabilities)])
      paste(
        sprintf("%s %.0f%%", option_labels, probabilities * 100),
        collapse = ", "
      )
    }

    tagList(
      h4("Sugestão do Jev"),
      p(sprintf(
        "Dificuldade: %s (confiança %.0f%%; %s)",
        difficulty_labels[[classification$difficulty$choice]],
        classification$difficulty$confidence * 100,
        probability_text(difficulty_probabilities, difficulty_labels)
      )),
      p(sprintf(
        "Variância: %s (confiança %.0f%%; %s)",
        variance_labels[[classification$variance$choice]],
        classification$variance$confidence * 100,
        probability_text(variance_probabilities, variance_labels)
      ))
    )
  })

  output$actionResult <- renderUI({
    result <- action_result()
    if (is.null(result)) {
      return(tags$pre("Nenhuma ação rodada."))
    }

    tags$pre(sprintf(
      paste(
        "Rolagem: %.2f",
        "Esperado (μ): %.2f | Diferença: %+.2f",
        "Percentil acumulado do modelo d100: %.2f%%",
        "Desvio padronizado: %+.2fσ | Percentil normal de referência: %.2f%%",
        "Faixa sigma: %s — %s",
        sep = "\n"
      ),
      result$roll,
      result$expected_roll,
      result$roll_delta,
      result$percentile * 100,
      result$sigma_score,
      result$normal_percentile * 100,
      result$sigma_band,
      result$sigma_adjective
    ))
  })

  output$narration <- renderUI({
    tags$p(narration() %||% "A narração aparece após a rolagem.")
  })

  output$plotDistribution <- renderPlot({
    mu <- suppressWarnings(as.numeric(input$mu))
    sigma <- suppressWarnings(as.numeric(input$sigma))
    validate(
      need(length(mu) == 1L && is.finite(mu), "Informe uma média finita."),
      need(mu >= 0 && mu <= 100, "A dificuldade deve estar entre 0 e 100."),
      need(length(sigma) == 1L && is.finite(sigma) && sigma > 0,
           "Informe um desvio padrão maior que zero.")
    )

    rolls <- seq(0, 100, length.out = 601)
    curve_data <- data.frame(
      roll = rolls,
      percentile = d100_percentile(rolls, mu, sigma) * 100
    )
    sigma_rolls <- c(
      0,
      pmin(pmax(mu + sigma * (-5:5), 0), 100),
      100
    )
    sigma_colors <- stats::setNames(
      grDevices::hcl.colors(length(sigma_band_labels), "RdYlBu", rev = TRUE),
      sigma_band_labels
    )
    sigma_regions <- data.frame(
      sigma_band = factor(
        sigma_band_labels,
        levels = sigma_band_labels
      ),
      xmin = sigma_rolls[-length(sigma_rolls)],
      xmax = sigma_rolls[-1]
    )
    result <- action_result()
    plot <- ggplot(curve_data, aes(x = roll, y = percentile)) +
      geom_rect(
        data = sigma_regions,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 100, fill = sigma_band),
        inherit.aes = FALSE,
        alpha = 0.16,
        colour = NA
      ) +
      geom_line(colour = "#35A9D6", linewidth = 1.2) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(value) paste0(value, "%"),
        expand = c(0, 0)
      ) +
      scale_fill_manual(
        name = "Faixas de z em relação a μ (percentil normal de referência)",
        values = sigma_colors,
        breaks = sigma_band_labels,
        labels = paste(sigma_adjectives, sigma_band_labels, sep = " — "),
        drop = FALSE
      ) +
      labs(
        title = "Distribuição acumulada do d100",
        x = "Resultado da rolagem (0–100)",
        y = "Frequência acumulada"
      ) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8)
      ) +
      guides(fill = guide_legend(
        ncol = 2,
        byrow = TRUE,
        override.aes = list(alpha = 0.7)
      ))

    if (!is.null(result)) {
      plot <- plot +
        geom_vline(
          xintercept = result$roll,
          colour = "#292929",
          linewidth = 1
        ) +
        geom_point(
          data = data.frame(
            roll = result$roll,
            percentile = result$percentile * 100
          ),
          aes(x = roll, y = percentile),
          inherit.aes = FALSE,
          colour = "#292929",
          size = 3
        )
    }

    plot
  })
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

shinyApp(ui, server)
