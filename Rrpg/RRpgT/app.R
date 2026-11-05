library(shiny)
library(ggplot2)
library(httr2)

env_file <- file.path(getwd(), ".env")
if (file.exists(env_file) && !readRenviron(env_file)) {
  stop("Não foi possível carregar o arquivo .env.", call. = FALSE)
}

difficulty_labels <- c(
  impossible = "Impossível",
  very_hard = "Muito difícil",
  hard = "Difícil",
  medium = "Médio",
  easy = "Fácil",
  very_easy = "Muito fácil",
  trivial = "Trivial"
)

difficulty_mu <- c(
  impossible = 97,
  very_hard = 85,
  hard = 70,
  medium = 50,
  easy = 30,
  very_easy = 15,
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

success_threshold_for_difficulty <- function(difficulty_score) {
  if (length(difficulty_score) != 1L || !is.numeric(difficulty_score) ||
      !is.finite(difficulty_score) || difficulty_score < 0 ||
      difficulty_score > 100) {
    stop("A dificuldade deve ser um valor entre 0 e 100.", call. = FALSE)
  }

  0.06 + 0.84 * difficulty_score / 100
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

classify_action_outcome <- function(percentile, difficulty_score) {
  if (length(percentile) != 1L || !is.numeric(percentile) ||
      !is.finite(percentile) || percentile < 0 || percentile > 1) {
    stop("O percentil deve ser um único valor entre 0 e 1.", call. = FALSE)
  }

  success_threshold <- success_threshold_for_difficulty(difficulty_score)
  if (percentile < 0.05) {
    "Crítico"
  } else if (percentile > 0.95) {
    "Extremo"
  } else if (percentile >= success_threshold) {
    "Sucesso"
  } else {
    "Falha"
  }
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
  success_threshold <- success_threshold_for_difficulty(mu)

  list(
    roll = roll,
    percentile = percentile,
    success_threshold = success_threshold,
    outcome = classify_action_outcome(percentile, mu)
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
    impossible = "Quase impossível; dificuldade-base 97.",
    very_hard = "Muito difícil; dificuldade-base 85.",
    hard = "Difícil; dificuldade-base 70.",
    medium = "Desafio mediano; dificuldade-base 50.",
    easy = "Fácil; dificuldade-base 30.",
    very_easy = "Muito fácil; dificuldade-base 15.",
    trivial = "Trivial; dificuldade-base 3."
  )
  variance_criteria <- list(
    chaotic = "Resultado muito imprevisível; desvio padrão-base 20.",
    erratic = "Resultado bastante imprevisível; desvio padrão-base 15.",
    medium = "Variabilidade moderada; desvio padrão-base 10.",
    expected = "Resultado relativamente previsível; desvio padrão-base 5.",
    predictable = "Resultado muito previsível; desvio padrão-base 1."
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
          "Classifique a variabilidade plausível dos resultados desta ação.",
          "Considere o quanto fatores e circunstâncias tornam o resultado imprevisível."
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

narrate_action_with_ollama <- function(result) {
  user_message <- paste(
    "Descreva em português brasileiro, em um parágrafo curto, o que ocorreu nesta ação.",
    "Use o evento e o resultado mecânico abaixo como fatos.",
    "Não altere a classificação nem invente regras ou consequências não sustentadas pelo contexto.",
    "",
    paste("Evento:", result$event),
    paste("Dificuldade:", result$difficulty),
    paste("Percentil mínimo para sucesso normal:", format(round(result$success_threshold * 100, 2), nsmall = 2), "%"),
    paste("Variância:", result$variance),
    paste("Valor rolado:", format(round(result$roll, 2), nsmall = 2)),
    paste("Percentil:", format(round(result$percentile * 100, 2), nsmall = 2), "%"),
    paste("Resultado:", result$outcome),
    sep = "\n"
  )

  response <- httr2::request("http://127.0.0.1:11434/api/chat") |>
    httr2::req_body_json(list(
      model = "gemma4:latest",
      messages = list(
        list(
          role = "system",
          content = paste(
            "Você narra resultados de ações em um RPG de mesa.",
            "Responda em português brasileiro, de forma envolvente e concisa.",
            "O código já determinou o resultado usando o percentil canônico e a dificuldade; siga essa classificação exatamente.",
            "A dificuldade é convertida linearmente em um limiar de sucesso entre 6% e 90%: limiar = 6% + 84% × dificuldade / 100. Assim, dificuldade 97 exige percentil de pelo menos 87,48%; dificuldade 85 exige 77,40%; dificuldade 70 exige 64,80%; dificuldade 50 exige 48%; dificuldade 30 exige 31,20%; dificuldade 15 exige 18,60%; dificuldade 3 exige 8,52%. Isso garante pelo menos 5 pontos percentuais de faixa de Sucesso antes do limite canônico de Extremo (>95%), mesmo na dificuldade máxima. O limiar exato desta ação está informado na mensagem do usuário.",
            "Marcos canônicos têm prioridade sobre o limiar: percentil < 5% é sempre Crítico (fracasso catastrófico), e percentil > 95% é sempre Extremo (sucesso espetacular). Entre 5% e o limiar de dificuldade é Falha; a partir do limiar até 95% é Sucesso. Assim, a ordem crescente dos resultados é Crítico, Falha, Sucesso e Extremo. Se o limiar for inferior a 5%, não há falha normal; se for igual ou superior a 95%, não há sucesso normal.",
            "Se o resultado for Crítico: comece deixando explícito que o personagem falhou no objetivo imediato e narre algo terrível e sério, contrário ao que queria. Nunca narre sucesso, alívio ou sorte.",
            "Se o resultado for Falha: comece dizendo claramente que o personagem não alcançou o objetivo descrito no evento. Narre um revés comum; progresso parcial não conta como atingir o objetivo. Não transforme a falha em sucesso e não imponha uma catástrofe de Crítico.",
            "Se o resultado for Sucesso: comece dizendo claramente que o personagem alcançou o objetivo descrito. Narre um sucesso normal, sem catástrofe nem benefício extraordinário.",
            "Se o resultado for Extremo: deixe claro que o personagem alcançou o objetivo de forma espetacular e obteve uma vantagem adicional extraordinária.",
            "Use o evento para determinar o objetivo e consequências plausíveis. Não contradiga o resultado calculado nem invente detalhes incompatíveis com o contexto."
          )
        ),
        list(
          role = "user",
          content = paste(
            user_message,
            "",
            "Siga a regra narrativa da classificação indicada. Mostre claramente a consequência correspondente na narração.",
            sep = "\n"
          )
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
      uiOutput("tailInfo"),
      plotOutput("plotDistribution", height = "500px"),
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
      difficulty = input$selectDifficulty,
      variance = input$selectVariance,
      mu = mu,
      sigma = sigma,
      roll = action$roll,
      percentile = action$percentile,
      success_threshold = action$success_threshold,
      outcome = action$outcome
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
      "Rolagem: %.2f\nPercentil: %.2f%%\nPercentil mínimo para sucesso normal: %.2f%%\nResultado: %s",
      result$roll,
      result$percentile * 100,
      result$success_threshold * 100,
      result$outcome
    ))
  })

  output$narration <- renderUI({
    tags$p(narration() %||% "A narração aparece após a rolagem.")
  })

  output$tailInfo <- renderUI({
    mu <- suppressWarnings(as.numeric(input$mu))
    sigma <- suppressWarnings(as.numeric(input$sigma))
    if (length(mu) != 1L || !is.finite(mu) ||
        length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
      return(tags$p("Informe parâmetros válidos para visualizar a distribuição."))
    }

    success_threshold <- success_threshold_for_difficulty(mu)
    normal_success_start <- success_threshold
    tags$p(sprintf(
      paste(
        "Crítico: < 5%% (resultado catastrófico) |",
        "Falha normal: 5%% a %.2f%% |",
        "Sucesso normal: %.2f%% a 95%% |",
        "Extremo: > 95%% (sucesso espetacular)"
      ),
      normal_success_start * 100,
      normal_success_start * 100
    ))
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
    success_threshold <- success_threshold_for_difficulty(mu)
    success_percentile <- success_threshold * 100
    critical_limit <- d100_roll_for_percentile(0.05, mu, sigma)
    success_limit <- d100_roll_for_percentile(success_percentile / 100, mu, sigma)
    extreme_limit <- d100_roll_for_percentile(0.95, mu, sigma)
    outcome_colors <- c(
      "Crítico" = "#F24343",
      "Falha" = "#ED8936",
      "Sucesso" = "#38A169",
      "Extremo" = "#3182CE"
    )
    outcome_regions <- data.frame(
      outcome = factor(
        c("Crítico", "Falha", "Sucesso", "Extremo"),
        levels = names(outcome_colors)
      ),
      xmin = c(
        0,
        critical_limit,
        success_limit,
        extreme_limit
      ),
      xmax = c(
        critical_limit,
        success_limit,
        extreme_limit,
        100
      )
    )
    result <- action_result()
    plot <- ggplot(curve_data, aes(x = roll, y = percentile)) +
      geom_rect(
        data = outcome_regions,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 100, fill = outcome),
        inherit.aes = FALSE,
        alpha = 0.14,
        colour = NA
      ) +
      geom_line(colour = "#35A9D6", linewidth = 1.2) +
      geom_hline(
        yintercept = c(5, success_percentile, 95),
        linetype = "dotted",
        colour = "grey55"
      ) +
      geom_vline(
        xintercept = critical_limit,
        linetype = "dashed",
        colour = "#F24343"
      ) +
      geom_vline(
        xintercept = extreme_limit,
        linetype = "dashed",
        colour = "#3182CE"
      ) +
      geom_vline(
        xintercept = success_limit,
        linetype = "dotted",
        colour = "#276749"
      ) +
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
        name = "Faixa de resultado",
        values = outcome_colors,
        breaks = names(outcome_colors),
        labels = c(
          "Crítico — fracasso catastrófico (< 5%)",
          sprintf("Falha — objetivo não alcançado (5–%.2f%%)", success_percentile),
          sprintf("Sucesso — objetivo alcançado (%.2f–95%%)", success_percentile),
          "Extremo — sucesso espetacular (> 95%)"
        ),
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
        legend.text = element_text(size = 9)
      ) +
      guides(fill = guide_legend(
        ncol = 2,
        byrow = TRUE,
        override.aes = list(alpha = 0.4)
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
