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

campaign_context_seed <- paste(
  "Vocês são um pequeno grupo de aventureiros que acabou de chegar à vila de Pedra Clara, no fim de uma estrada cercada por mata.",
  "Na estalagem, a dona, Mara, conta que uma caravana desapareceu na noite passada e pede ajuda para encontrar o mercador Raul e seus viajantes.",
  "Lá fora, a chuva ameaça apagar os rastros da estrada.",
  sep = "\n"
)

campaign_memory_limit <- 8L

format_campaign_context <- function(memory) {
  paste(
    c(campaign_context_seed, unlist(memory, use.names = FALSE)),
    collapse = "\n\n"
  )
}

append_campaign_memory <- function(memory, event, narration_text) {
  entry <- paste(
    paste("Ação:", trimws(event)),
    paste("O que aconteceu:", trimws(narration_text)),
    sep = "\n"
  )
  utils::tail(c(memory, list(entry)), campaign_memory_limit)
}

format_jev_state <- function(event_description, campaign_context) {
  paste(
    "CONTEXTO DA AVENTURA — fatos já estabelecidos:",
    campaign_context,
    "",
    "AÇÃO ATUAL — classifique esta ação levando em conta o contexto acima:",
    event_description,
    sep = "\n"
  )
}

difficulty_success_band_index <- stats::setNames(
  rev(seq_along(sigma_adjectives)),
  names(difficulty_labels)
)

difficulty_success_thresholds <- stats::setNames(
  vapply(
    difficulty_success_band_index,
    function(index) {
      paste(sigma_adjectives[[index]], sigma_band_labels[[index]], sep = " — ")
    },
    character(1)
  ),
  names(difficulty_success_band_index)
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

format_result_number <- function(value) {
  formatted <- sprintf("%.15f", value)
  formatted <- sub("0+$", "", formatted)
  formatted <- sub("\\.$", "", formatted)
  if (formatted %in% c("-0", "")) "0" else formatted
}

format_signed_result_number <- function(value) {
  formatted <- format_result_number(value)
  if (value > 0) paste0("+", formatted) else formatted
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
  sigma_score <- stats::qnorm(percentile)
  expected_sigma_delta <- (roll - mu) / sigma

  list(
    roll = roll,
    percentile = percentile,
    is_typical = sigma_score >= -1 && sigma_score <= 1,
    expected_roll = mu,
    roll_delta = roll - mu,
    expected_sigma_delta = expected_sigma_delta,
    sigma_score = sigma_score,
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

classify_event_with_jev <- function(event_description, campaign_context) {
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
    state = format_jev_state(event_description, campaign_context),
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
    "CONTEXTO ANTERIOR DA AVENTURA — preserve a continuidade e não contradiga fatos estabelecidos:",
    result$campaign_context,
    "",
    "AÇÃO ATUAL:",
    result$event,
    paste(
      "Desfecho:",
      if (result$success) {
        "o personagem alcança o objetivo."
      } else {
        "o personagem não alcança o objetivo."
      }
    ),
    sep = "\n"
  )
}

stream_ollama_response <- function(
  system_prompt,
  user_prompt,
  on_token,
  on_complete,
  on_error,
  register_cancel,
  is_cancelled,
  empty_response_message = "Ollama retornou uma resposta vazia ou em formato inválido."
) {
  request <- httr2::request("http://127.0.0.1:11434/api/chat") |>
    httr2::req_body_json(list(
      model = "gemma4:latest",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      stream = TRUE
    )) |>
    httr2::req_timeout(180)
  response <- NULL
  chunks <- character()

  close_response <- function() {
    if (!is.null(response)) {
      close(response)
      response <<- NULL
    }
  }

  cancel <- function() {
    cancelled <<- TRUE
    close_response()
  }
  cancelled <- FALSE
  register_cancel(cancel)

  fail <- function(error) {
    close_response()
    if (!cancelled && !is_cancelled()) {
      on_error(conditionMessage(error))
    }
  }

  tryCatch({
    if (cancelled || is_cancelled()) {
      return(invisible(NULL))
    }
    response <- httr2::req_perform_connection(request, blocking = FALSE)
    httr2::resp_check_status(response)
  }, error = fail)

  if (cancelled || is_cancelled() || is.null(response)) {
    close_response()
    return(invisible(NULL))
  }

  read_next <- function() {
    if (cancelled || is_cancelled()) {
      close_response()
      return(invisible(NULL))
    }

    tryCatch({
      lines <- httr2::resp_stream_lines(response, lines = 1)
      if (length(lines) == 0L) {
        if (httr2::resp_stream_is_complete(response)) {
          stop("Ollama encerrou o streaming sem concluir a resposta.",
               call. = FALSE)
        }
        later::later(read_next, delay = 0.02)
        return(invisible(NULL))
      }

      line <- lines[[1]]
      if (nzchar(trimws(line))) {
        item <- jsonlite::fromJSON(line, simplifyVector = FALSE)
        if (cancelled || is_cancelled()) {
          close_response()
          return(invisible(NULL))
        }
        if (!is.null(item$error)) {
          stop(paste("Ollama retornou um erro:", item$error), call. = FALSE)
        }

        token <- item$message$content
        if (is.character(token) && length(token) == 1L &&
            !is.na(token) && nzchar(token)) {
          chunks <<- c(chunks, token)
          on_token(token)
        }

        if (isTRUE(item$done)) {
          close_response()
          if (cancelled || is_cancelled()) {
            return(invisible(NULL))
          }
          full_response <- paste0(chunks, collapse = "")
          if (!nzchar(trimws(full_response))) {
            stop(empty_response_message, call. = FALSE)
          }
          on_complete(full_response)
          return(invisible(NULL))
        }
      }

      later::later(read_next, delay = 0.02)
    }, error = fail)
  }

  later::later(read_next, delay = 0)
  invisible(NULL)
}

narrate_action_with_ollama <- function(
  result,
  on_token,
  on_complete,
  on_error,
  register_cancel,
  is_cancelled
) {
  stream_ollama_response(
    system_prompt = paste(
      "Você é o Mestre de Jogo e está narrando uma cena de RPG em andamento. Faça roleplay: fale como quem conduz a cena para os jogadores, não como alguém que avalia a ficha ou comenta uma jogada.",
      "Use o contexto anterior como memória da aventura e as informações da ação atual somente como orientação interna. Continue a história a partir do que já aconteceu, mantenha personagens, lugares e fatos consistentes e não reinicie a aventura.",
      "Responda com a cena acontecendo agora: descreva ações, gestos, expressões, sons, ambiente e consequências que os personagens percebem.",
      "Não explique nem avalie a capacidade, o desempenho, a persuasão ou a convicção do personagem. Não dê notas, níveis, categorias ou rótulos à ação ou ao personagem.",
      "Não mencione regras, jogo, rolagem, dados, resultados, atributos, perícias, dificuldade, sorte, probabilidade ou qualquer outra mecânica. Não escreva cabeçalhos nem anuncie SUCESSO ou FALHA: mostre o desfecho somente pelo que acontece na ficção.",
      "Escreva em português brasileiro coloquial, como uma fala teatral e vívida do DM. Quando couber, interprete os personagens com falas curtas em discurso direto. Prefira duas ou três frases curtas e não invente detalhes que contradigam a situação.",
      "Mantenha o desfecho indicado nas informações internas, mas não o nomeie nem o explique; narre apenas a consequência vivida pelos personagens.",
      sep = "\n"
    ),
    user_prompt = action_narration_facts(result),
    on_token = on_token,
    on_complete = on_complete,
    on_error = on_error,
    register_cancel = register_cancel,
    is_cancelled = is_cancelled,
    empty_response_message = "Ollama retornou uma narração vazia ou em formato inválido."
  )
}

campaign_narration_prompts <- function(campaign_context) {
  list(
    system = paste(
      "Você é o Mestre de Jogo de uma aventura de fantasia em andamento.",
      "Continue a história a partir do contexto fornecido. Narre um novo momento interessante e coerente, sem recontar o resumo, sem decidir ações pelos jogadores e sem resolver por eles escolhas ainda abertas.",
      "Use roleplay teatral em português brasileiro coloquial: descreva o que os personagens veem, ouvem e sentem, e inclua falas curtas de personagens quando fizer sentido.",
      "Não mencione jogo, regras, rolagens, atributos ou qualquer jargão técnico. Escreva de duas a quatro frases, deixando uma situação ou oportunidade clara para os jogadores responderem.",
      sep = "\n"
    ),
    user = paste(
      "CONTEXTO ATUAL DA AVENTURA:",
      campaign_context,
      "",
      "Continue a cena agora, sem decidir o que os aventureiros fazem.",
      sep = "\n"
    )
  )
}

character_action_prompts <- function(campaign_context) {
  list(
    system = paste(
      "Você interpreta um dos aventureiros jogadores desta história. Leia o contexto da aventura e proponha uma ação que esse personagem poderia tentar neste momento.",
      "Responda somente com uma frase curta em primeira pessoa, descrevendo uma intenção concreta para preencher o campo de ação. Não narre o resultado, não faça a ação acontecer e não inclua explicações, alternativas, aspas ou comentários.",
      "Mantenha a proposta coerente com o que já aconteceu e não invente poderes, objetos ou informações pessoais que não estejam estabelecidos.",
      sep = "\n"
    ),
    user = paste(
      "CONTEXTO ATUAL DA AVENTURA:",
      campaign_context,
      "",
      "Escreva uma ação que meu personagem tenta realizar agora.",
      sep = "\n"
    )
  )
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
      uiOutput("campaignStory"),
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
      br(),
      br(),
      actionButton("simulateCharacterAction", "Simular ação de personagem"),
      hr(),
      uiOutput("statusText"),
      uiOutput("campaignContext"),
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
      div(
        style = "display:flex;gap:8px;",
        actionButton("rollAction", "Rodar ação", class = "btn-primary"),
        actionButton("cancelAction", "Cancelar", class = "btn-warning", disabled = TRUE)
      ),
      hr()
    ),
    column(
      width = 8,
      h3("Distribuição dos resultados"),
      plotOutput("plotDistribution", height = "590px"),
      hr(),
      div(
        style = "display:flex;align-items:center;gap:10px;",
        h4("Narração", style = "margin:0;"),
        actionButton("generateNarrative", "Gerar Narrativa")
      ),
      br(),
      uiOutput("narration"),
      h4("Resultado da ação"),
      uiOutput("actionResult")
    )
  )
)

server <- function(input, output, session) {
  jev_result <- reactiveVal(NULL)
  action_result <- reactiveVal(NULL)
  narration <- reactiveVal(NULL)
  status_message <- reactiveVal("Descreva o evento e peça uma análise ao Jev.")
  generation_running <- reactiveVal(FALSE)
  generation_id <- reactiveVal(0L)
  active_generation <- reactiveVal(NULL)
  active_cancel <- reactiveVal(NULL)
  campaign_memory <- reactiveVal(list())
  campaign_narrative <- reactiveVal(character())
  campaign_story_stream <- reactiveVal(NULL)

  set_generation_running <- function(running) {
    generation_running(running)
    updateActionButton(session, "cancelAction", disabled = !running)
  }

  cancel_active_generation <- function(
    message = "Geração cancelada. O resultado da rolagem foi mantido.",
    flush = TRUE
  ) {
    if (!isTRUE(shiny::isolate(generation_running()))) {
      return(invisible(NULL))
    }

    cancel <- shiny::isolate(active_cancel())
    active_generation(NULL)
    active_cancel(NULL)
    set_generation_running(FALSE)
    if (is.function(cancel)) {
      cancel()
    }
    if (!is.null(message)) {
      status_message(message)
    }
    if (flush && !session$isClosed()) {
      session$flushOutput()
    }
    invisible(NULL)
  }

  observeEvent(input$cancelAction, {
    cancel_active_generation()
  })

  start_streaming_generation <- function(
    start_stream,
    on_token,
    on_complete,
    on_error
  ) {
    current_generation <- generation_id() + 1L
    generation_id(current_generation)
    active_generation(current_generation)
    active_cancel(NULL)
    set_generation_running(TRUE)

    is_current_generation <- function() {
      identical(
        shiny::isolate(active_generation()),
        current_generation
      )
    }

    session$onFlushed(function() {
      later::later(function() {
        if (!is_current_generation()) {
          return()
        }

        start_stream(
          on_token = function(token) {
            if (!is_current_generation()) {
              return()
            }
            on_token(token)
            session$flushOutput()
          },
          on_complete = function(response_text) {
            if (!is_current_generation()) {
              return()
            }
            active_generation(NULL)
            active_cancel(NULL)
            set_generation_running(FALSE)
            on_complete(response_text)
            session$flushOutput()
          },
          on_error = function(message) {
            if (!is_current_generation()) {
              return()
            }
            active_generation(NULL)
            active_cancel(NULL)
            set_generation_running(FALSE)
            on_error(message)
            session$flushOutput()
          },
          register_cancel = function(cancel) {
            active_cancel(cancel)
          },
          is_cancelled = function() {
            !is_current_generation()
          }
        )
      }, delay = 0.05)
    }, once = TRUE)
  }

  observeEvent(input$generateNarrative, {
    cancel_active_generation(message = NULL, flush = FALSE)
    context_snapshot <- format_campaign_context(campaign_memory())
    prompts <- campaign_narration_prompts(context_snapshot)
    campaign_story_stream("")
    status_message("Gerando continuação da aventura...")

    start_streaming_generation(
      start_stream = function(...) {
        stream_ollama_response(
          system_prompt = prompts$system,
          user_prompt = prompts$user,
          ...,
          empty_response_message = "Ollama retornou uma continuação vazia."
        )
      },
      on_token = function(token) {
        campaign_story_stream(paste0(
          shiny::isolate(campaign_story_stream()),
          token
        ))
      },
      on_complete = function(response_text) {
        campaign_memory(append_campaign_memory(
          shiny::isolate(campaign_memory()),
          "Continuação da narrativa",
          response_text
        ))
        campaign_narrative(utils::tail(
          c(shiny::isolate(campaign_narrative()), response_text),
          campaign_memory_limit
        ))
        campaign_story_stream(NULL)
        status_message("Narrativa atualizada.")
      },
      on_error = function(message) {
        status_message(paste("Não foi possível gerar a narrativa:", message))
      }
    )
  })

  observeEvent(input$simulateCharacterAction, {
    cancel_active_generation(message = NULL, flush = FALSE)
    prompts <- character_action_prompts(
      format_campaign_context(campaign_memory())
    )
    status_message("Criando uma sugestão de ação para um personagem...")

    start_streaming_generation(
      start_stream = function(...) {
        stream_ollama_response(
          system_prompt = prompts$system,
          user_prompt = prompts$user,
          ...,
          empty_response_message = "Ollama não sugeriu uma ação."
        )
      },
      on_token = function(token) invisible(NULL),
      on_complete = function(response_text) {
        updateTextAreaInput(
          session,
          "eventDescription",
          value = trimws(response_text)
        )
        status_message("Ação sugerida no campo. Revise e analise com Jev quando quiser.")
      },
      on_error = function(message) {
        status_message(paste("Não foi possível sugerir uma ação:", message))
      }
    )
  })

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
    context_snapshot <- format_campaign_context(campaign_memory())
    result <- tryCatch(
      classify_event_with_jev(event, context_snapshot),
      error = function(error) {
        status_message(paste("Não foi possível classificar o evento:", conditionMessage(error)))
        NULL
      }
    )
    if (is.null(result)) {
      return()
    }

    jev_result(list(
      event = event,
      classification = result,
      campaign_context = context_snapshot
    ))
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
    cancel_active_generation(message = NULL, flush = FALSE)
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
      campaign_context = analysis$campaign_context,
      mu = mu,
      sigma = sigma,
      roll = action$roll,
      success_threshold = unname(difficulty_success_thresholds[[input$selectDifficulty]]),
      success_threshold_index = unname(difficulty_success_band_index[[input$selectDifficulty]]),
      percentile = action$percentile,
      is_typical = action$is_typical,
      expected_roll = action$expected_roll,
      roll_delta = action$roll_delta,
      expected_sigma_delta = action$expected_sigma_delta,
      sigma_score = action$sigma_score,
      sigma_band_index = match(action$sigma_adjective, sigma_adjectives),
      sigma_band = action$sigma_band,
      sigma_adjective = action$sigma_adjective
    )
    result$success <- result$sigma_band_index >= result$success_threshold_index
    result$outcome <- if (result$success) "SUCESSO" else "FALHA"
    action_result(result)
    narration(NULL)
    campaign_story_stream("")
    status_message("Rolagem exibida no gráfico. Gerando a narração local...")
    start_streaming_generation(
      start_stream = function(...) {
        narrate_action_with_ollama(
          result,
          ...
        )
      },
      on_token = function(token) {
        narration(paste0(shiny::isolate(narration()), token))
        campaign_story_stream(paste0(
          shiny::isolate(campaign_story_stream()),
          token
        ))
      },
      on_complete = function(response_text) {
        campaign_memory(append_campaign_memory(
          shiny::isolate(campaign_memory()),
          result$event,
          response_text
        ))
        campaign_narrative(utils::tail(
          c(shiny::isolate(campaign_narrative()), response_text),
          campaign_memory_limit
        ))
        campaign_story_stream(NULL)
        status_message("Ação e narração concluídas.")
      },
      on_error = function(message) {
        status_message(paste(
          "A ação foi calculada, mas a narração falhou:",
          message
        ))
      }
    )
  })

  session$onSessionEnded(function() {
    cancel_active_generation(message = NULL, flush = FALSE)
  })

  output$statusText <- renderUI(tags$p(status_message()))

  output$campaignStory <- renderUI({
    completed_narration <- campaign_narrative()
    current_narration <- campaign_story_stream()
    show_current <- !is.null(current_narration) &&
      nzchar(current_narration) &&
      (length(completed_narration) == 0L ||
         !identical(tail(completed_narration, 1L), current_narration))

    tags$section(
      h2("Aventura até agora"),
      tags$div(
        style = paste(
          "max-height:320px;overflow-y:auto;padding:12px;",
          "border:1px solid #ddd;border-radius:4px;margin-bottom:16px;"
        ),
        tags$p(campaign_context_seed),
        lapply(completed_narration, tags$p),
        if (show_current) tags$p(current_narration)
      )
    )
  })

  output$campaignContext <- renderUI({
    tags$details(
      tags$summary("Contexto da aventura"),
      tags$pre(
        format_campaign_context(campaign_memory()),
        style = "white-space:pre-wrap;"
      )
    )
  })

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

    tags$details(
      tags$summary("Sugestão do Jev"),
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

    tags$div(
      tags$h3(
        result$outcome,
        style = paste0(
          "color:",
          if (result$success) "#237a3b" else "#b42318",
          ";font-weight:bold;"
        )
      ),
      tags$pre(paste(
        paste("Rolagem:", format_result_number(result$roll)),
        paste(
          "Esperado (μ):", format_result_number(result$expected_roll),
          "| Diferença:", format_signed_result_number(result$roll_delta)
        ),
        paste(
          "Percentil acumulado da curva d100:",
          paste0(format_result_number(result$percentile * 100), "%")
        ),
        paste(
          "Na faixa central típica de 68,27% da distribuição:",
          if (result$is_typical) "Sim" else "Não"
        ),
        paste(
          "Distância do centro esperado em unidades de σ:",
          paste0(format_signed_result_number(result$expected_sigma_delta), "σ")
        ),
        paste(
          "Desvio normal-equivalente da raridade:",
          paste0(format_signed_result_number(result$sigma_score), "σ")
        ),
        paste("Índice da faixa observada:", result$sigma_band_index, "de 12"),
        paste("Faixa mínima de sucesso:", result$success_threshold),
        paste("Índice mínimo para sucesso:", result$success_threshold_index, "de 12"),
        paste("Faixa sigma:", result$sigma_band, "—", result$sigma_adjective),
        sep = "\n"
      ))
    )
  })

  output$narration <- renderUI({
    current_narration <- narration()
    if (!is.null(current_narration)) {
      return(tags$p(current_narration))
    }

    if (is.null(action_result())) {
      return(tags$p("A resposta do DM aparece após a rolagem."))
    }

    if (isTRUE(generation_running())) {
      return(tags$p("Gerando Resposta do DM..."))
    }

    tags$p("Resposta do DM indisponível.")
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
      d100_roll_for_percentile(stats::pnorm(-5:5), mu, sigma),
      100
    )
    sigma_colors <- stats::setNames(
      grDevices::hcl.colors(length(sigma_band_labels), "RdYlBu"),
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
        name = NULL,
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

    success_threshold_index <- difficulty_success_band_index[[input$selectDifficulty]]
    if (!is.null(success_threshold_index)) {
      plot <- plot +
        geom_vline(
          xintercept = sigma_rolls[[success_threshold_index]],
          colour = "#292929",
          linetype = "dashed",
          linewidth = 0.9
        )
    }

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
