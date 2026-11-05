library(shiny)
library(ggplot2)
library(httr2)
library(DBI)
library(RSQLite)

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

ollama_base_url <- function() {
  base_url <- trimws(Sys.getenv(
    "OLLAMA_BASE_URL",
    unset = "http://127.0.0.1:11434"
  ))
  sub("/+$", "", base_url)
}

ollama_embedding_model <- function() {
  trimws(Sys.getenv(
    "OLLAMA_EMBEDDING_MODEL",
    unset = "nomic-embed-text:latest"
  ))
}

campaign_database_path <- function() {
  configured_path <- trimws(Sys.getenv(
    "RPG_VECTOR_DB_PATH",
    unset = file.path("data", "campaign_memory.sqlite")
  ))
  if (!nzchar(configured_path)) {
    stop("RPG_VECTOR_DB_PATH não pode ser vazio.", call. = FALSE)
  }

  if (!grepl("^(/|~)", configured_path)) {
    configured_path <- file.path(getwd(), configured_path)
  }
  path.expand(configured_path)
}

open_campaign_store <- function() {
  database_path <- campaign_database_path()
  database_directory <- dirname(database_path)
  if (!dir.exists(database_directory) &&
      !dir.create(database_directory, recursive = TRUE)) {
    stop("Não foi possível criar o diretório do banco da campanha.",
         call. = FALSE)
  }

  connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = database_path)
  DBI::dbExecute(connection, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(connection, "PRAGMA busy_timeout = 5000")
  DBI::dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS campaigns (
      id TEXT PRIMARY KEY,
      title TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")
  DBI::dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS campaign_memories (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      campaign_id TEXT NOT NULL REFERENCES campaigns(id),
      kind TEXT NOT NULL,
      content TEXT NOT NULL,
      metadata TEXT NOT NULL,
      embedding TEXT,
      embedding_model TEXT,
      created_at TEXT NOT NULL
    )
  ")
  DBI::dbExecute(connection, "
    CREATE INDEX IF NOT EXISTS campaign_memories_campaign_time
    ON campaign_memories(campaign_id, id)
  ")
  DBI::dbExecute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS campaign_memories_single_seed
    ON campaign_memories(campaign_id) WHERE kind = 'seed'
  ")
  connection
}

campaign_embedding_vectors <- function(texts) {
  model <- ollama_embedding_model()
  if (!nzchar(model)) {
    stop("OLLAMA_EMBEDDING_MODEL não pode ser vazio.", call. = FALSE)
  }

  response <- httr2::request(paste0(ollama_base_url(), "/api/embed")) |>
    httr2::req_body_json(list(model = model, input = as.list(texts))) |>
    httr2::req_timeout(120) |>
    httr2::req_perform()
  httr2::resp_check_status(response)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  embeddings <- body$embeddings
  if (!is.list(embeddings) || length(embeddings) != length(texts)) {
    stop("Ollama retornou uma quantidade inválida de vetores.",
         call. = FALSE)
  }

  vectors <- lapply(embeddings, function(embedding) {
    vector <- unlist(embedding, recursive = TRUE, use.names = FALSE)
    if (!is.numeric(vector) || length(vector) == 0L ||
        any(!is.finite(vector))) {
      stop("Ollama retornou um vetor de embedding inválido.",
           call. = FALSE)
    }
    vector
  })
  dimensions <- vapply(vectors, length, integer(1))
  if (length(unique(dimensions)) != 1L) {
    stop("Ollama retornou vetores com dimensões diferentes.",
         call. = FALSE)
  }
  vectors
}

ensure_campaign_seed <- function(connection, campaign_id) {
  now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  DBI::dbExecute(
    connection,
    paste(
      "INSERT OR IGNORE INTO campaigns (id, title, created_at, updated_at)",
      "VALUES (?, ?, ?, ?)"
    ),
    params = list(campaign_id, "Aventura de Pedra Clara", now, now)
  )

  seed_exists <- DBI::dbGetQuery(
    connection,
    "SELECT 1 FROM campaign_memories WHERE campaign_id = ? AND kind = 'seed' LIMIT 1",
    params = list(campaign_id)
  )
  if (nrow(seed_exists) == 0L) {
    DBI::dbExecute(
      connection,
      paste(
        "INSERT OR IGNORE INTO campaign_memories",
        "(campaign_id, kind, content, metadata, created_at)",
        "VALUES (?, 'seed', ?, ?, ?)"
      ),
      params = list(
        campaign_id,
        campaign_context_seed,
        as.character(jsonlite::toJSON(
          list(narration = campaign_context_seed),
          auto_unbox = TRUE
        )),
        now
      )
    )
  }
  invisible(NULL)
}

insert_campaign_memory <- function(
  connection,
  campaign_id,
  kind,
  content,
  metadata
) {
  now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO campaign_memories",
      "(campaign_id, kind, content, metadata, created_at)",
      "VALUES (?, ?, ?, ?, ?)"
    ),
    params = list(
      campaign_id,
      kind,
      content,
      as.character(jsonlite::toJSON(
        metadata,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      )),
      now
    )
  )
  as.integer(DBI::dbGetQuery(
    connection,
    "SELECT last_insert_rowid() AS id"
  )$id[[1]])
}

update_campaign_memory <- function(
  connection,
  memory_id,
  content,
  metadata
) {
  DBI::dbExecute(
    connection,
    paste(
      "UPDATE campaign_memories",
      "SET content = ?, metadata = ?, embedding = NULL, embedding_model = NULL",
      "WHERE id = ?"
    ),
    params = list(
      content,
      as.character(jsonlite::toJSON(
        metadata,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      )),
      memory_id
    )
  )
  if (DBI::dbGetQuery(
    connection,
    "SELECT changes() AS changed"
  )$changed[[1]] != 1L) {
    stop("O registro da campanha não foi encontrado para atualização.",
         call. = FALSE)
  }
  invisible(NULL)
}

index_pending_campaign_memories <- function(connection, campaign_id) {
  model <- ollama_embedding_model()
  pending <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT id, content FROM campaign_memories",
      "WHERE campaign_id = ?",
      "AND (embedding IS NULL OR embedding_model != ?)",
      "ORDER BY id"
    ),
    params = list(campaign_id, model)
  )
  if (nrow(pending) == 0L) {
    return(invisible(NULL))
  }

  vectors <- campaign_embedding_vectors(pending$content)
  DBI::dbWithTransaction(connection, {
    for (index in seq_len(nrow(pending))) {
      DBI::dbExecute(
        connection,
        paste(
          "UPDATE campaign_memories",
          "SET embedding = ?, embedding_model = ?",
          "WHERE id = ?"
        ),
        params = list(
          as.character(jsonlite::toJSON(
            as.list(vectors[[index]]),
            auto_unbox = FALSE,
            digits = NA
          )),
          model,
          pending$id[[index]]
        )
      )
    }
  })
  invisible(NULL)
}

decode_campaign_embedding <- function(encoded) {
  vector <- unlist(
    jsonlite::fromJSON(encoded, simplifyVector = TRUE),
    use.names = FALSE
  )
  if (!is.numeric(vector) || length(vector) == 0L ||
      any(!is.finite(vector))) {
    stop("O banco contém um vetor de embedding inválido.", call. = FALSE)
  }
  vector
}

retrieve_campaign_context <- function(
  connection,
  campaign_id,
  query,
  relevant_limit = 5L,
  recent_limit = 5L
) {
  ensure_campaign_seed(connection, campaign_id)
  index_pending_campaign_memories(connection, campaign_id)
  query_vectors <- campaign_embedding_vectors(query)
  query_norms <- vapply(query_vectors, function(vector) {
    sqrt(sum(vector^2))
  }, numeric(1))
  if (any(!is.finite(query_norms)) || any(query_norms == 0)) {
    stop("Um vetor da consulta não pode ter norma zero.", call. = FALSE)
  }
  memories <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT id, kind, content, embedding, embedding_model",
      "FROM campaign_memories WHERE campaign_id = ? ORDER BY id"
    ),
    params = list(campaign_id)
  )
  seed <- memories[memories$kind == "seed", , drop = FALSE]
  candidates <- memories[memories$kind != "seed", , drop = FALSE]
  if (nrow(seed) != 1L || anyNA(memories$embedding)) {
    stop("A memória da campanha ainda não está completamente indexada.",
         call. = FALSE)
  }

  selected <- candidates[FALSE, , drop = FALSE]
  if (nrow(candidates) > 0L) {
    candidate_vectors <- lapply(candidates$embedding, decode_campaign_embedding)
    similarity <- vapply(candidate_vectors, function(vector) {
      if (length(vector) != length(query_vectors[[1]])) {
        stop(
          "A dimensão de um vetor salvo não corresponde ao modelo de embedding atual.",
          call. = FALSE
        )
      }
      vector_norm <- sqrt(sum(vector^2))
      if (vector_norm == 0) return(0)
      max(vapply(seq_along(query_vectors), function(index) {
        sum(vector * query_vectors[[index]]) /
          (vector_norm * query_norms[[index]])
      }, numeric(1)))
    }, numeric(1))

    relevant_indices <- head(
      order(similarity, decreasing = TRUE),
      min(as.integer(relevant_limit), length(similarity))
    )
    recent_indices <- tail(
      seq_len(nrow(candidates)),
      min(as.integer(recent_limit), nrow(candidates))
    )
    selected_indices <- sort(unique(c(relevant_indices, recent_indices)))
    selected <- candidates[selected_indices, , drop = FALSE]
  }

  selected_lines <- if (nrow(selected) == 0L) {
    character()
  } else {
    paste0(
      "[", selected$kind, " #", selected$id, "]\n",
      selected$content
    )
  }
  paste(
    c(
      "CONTEXTO-BASE DA CAMPANHA:",
      seed$content[[1]],
      if (length(selected_lines) > 0L) {
        c("", "MEMÓRIAS RECUPERADAS — fatos relevantes e acontecimentos recentes:",
          selected_lines)
      }
    ),
    collapse = "\n"
  )
}

campaign_memory_archive <- function(connection, campaign_id) {
  DBI::dbGetQuery(
    connection,
    paste(
      "SELECT id, kind, content, metadata",
      "FROM campaign_memories WHERE campaign_id = ? ORDER BY id"
    ),
    params = list(campaign_id)
  )
}

campaign_story_entries <- function(connection, campaign_id) {
  memories <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT id, kind, metadata FROM campaign_memories",
      "WHERE campaign_id = ? AND kind IN ('action', 'narrative')",
      "ORDER BY id"
    ),
    params = list(campaign_id)
  )
  lapply(seq_len(nrow(memories)), function(index) {
    metadata <- jsonlite::fromJSON(
      memories$metadata[[index]],
      simplifyVector = FALSE
    )
    list(
      id = memories$id[[index]],
      kind = memories$kind[[index]],
      event = metadata$event %||% NULL,
      narration = metadata$narration %||% NULL
    )
  })
}

character_sheet_prompts <- function(campaign_context) {
  list(
    system = paste(
      "Você mantém a ficha do personagem principal de uma campanha de RPG.",
      "Use exclusivamente os fatos explicitamente presentes nas memórias fornecidas. Não invente nem deduza valores mecânicos. Diferencie o personagem principal dos companheiros e não transforme uma intenção em fato consumado; registre equipamento como possuído somente quando a narrativa confirmar que está com o personagem.",
      "A fala em primeira pessoa do jogador identifica o protagonista; afirmações como 'sou o/a [classe] [nome]' são evidência direta para os campos nome e classe. Use as menções ao deus ou à divindade feitas pelo próprio protagonista para preencher divindade.",
      "Quando um dado não estiver estabelecido, use null; para listas sem fatos confirmados, use []. Atualize a ficha considerando todas as memórias, preservando fatos anteriores que não foram contraditos.",
      "Responda somente com um objeto JSON válido, sem Markdown, comentários ou texto antes/depois. Use exatamente este esquema:",
      '{"name":null,"class":null,"ancestry":null,"background":null,"deity":null,"level":null,"hit_points":null,"attributes":{"strength":null,"dexterity":null,"constitution":null,"intelligence":null,"wisdom":null,"charisma":null},"skills":[],"abilities":[],"equipment":[],"companions":[],"other_facts":[]}',
      "Campos textuais devem ser strings ou null; level e hit_points devem ser números ou null; attributes deve conter exatamente os seis atributos, cada um numérico ou null; listas devem conter somente strings confirmadas.",
      "A ficha é do personagem principal, não de um companheiro. Mantenha nomes e descrições conforme registrados e escreva em português.",
      sep = "\n"
    ),
    user = paste(
      "Atualize a ficha do personagem principal com base nestas memórias recuperadas da base vetorial da campanha:",
      campaign_context,
      sep = "\n\n"
    )
  )
}

parse_character_sheet <- function(response_text) {
  sheet <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  required_fields <- c(
    "name", "class", "ancestry", "background", "deity", "level",
    "hit_points", "attributes", "skills", "abilities", "equipment",
    "companions", "other_facts"
  )
  if (!is.list(sheet) || !all(required_fields %in% names(sheet))) {
    stop("A resposta da LLM não contém todos os campos da ficha.",
         call. = FALSE)
  }

  text_fields <- c(
    "name", "class", "ancestry", "background", "deity"
  )
  invalid_text <- vapply(text_fields, function(field) {
    value <- sheet[[field]]
    !is.null(value) &&
      (!is.character(value) || length(value) != 1L || is.na(value))
  }, logical(1))
  if (any(invalid_text)) {
    stop("A LLM retornou um campo textual inválido na ficha.",
         call. = FALSE)
  }

  numeric_fields <- c("level", "hit_points")
  invalid_numeric <- vapply(numeric_fields, function(field) {
    value <- sheet[[field]]
    !is.null(value) &&
      (!is.numeric(value) || length(value) != 1L || !is.finite(value))
  }, logical(1))
  if (any(invalid_numeric)) {
    stop("A LLM retornou um valor numérico inválido na ficha.",
         call. = FALSE)
  }

  attribute_fields <- c(
    "strength", "dexterity", "constitution",
    "intelligence", "wisdom", "charisma"
  )
  attributes <- sheet$attributes
  if (!is.list(attributes) ||
      !all(attribute_fields %in% names(attributes))) {
    stop("A LLM retornou atributos incompletos na ficha.",
         call. = FALSE)
  }
  invalid_attributes <- vapply(attribute_fields, function(field) {
    value <- attributes[[field]]
    !is.null(value) &&
      (!is.numeric(value) || length(value) != 1L || !is.finite(value))
  }, logical(1))
  if (any(invalid_attributes)) {
    stop("A LLM retornou um atributo inválido na ficha.",
         call. = FALSE)
  }

  list_fields <- c(
    "skills", "abilities", "equipment", "companions", "other_facts"
  )
  invalid_lists <- vapply(list_fields, function(field) {
    value <- sheet[[field]]
    !is.list(value) ||
      any(!vapply(value, function(item) {
        is.character(item) && length(item) == 1L && !is.na(item)
      }, logical(1)))
  }, logical(1))
  if (any(invalid_lists)) {
    stop("A LLM retornou uma lista inválida na ficha.",
         call. = FALSE)
  }
  sheet
}

campaign_memory_summary <- function(connection, campaign_id) {
  DBI::dbGetQuery(
    connection,
    paste(
      "SELECT COUNT(*) AS total,",
      "SUM(CASE WHEN embedding IS NOT NULL AND embedding_model = ?",
      "THEN 1 ELSE 0 END) AS indexed",
      "FROM campaign_memories WHERE campaign_id = ?"
    ),
    params = list(ollama_embedding_model(), campaign_id)
  )
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

parse_jev_answers <- function(answers) {
  if (!is.list(answers) || is.null(answers$coherence)) {
    stop("A resposta do Jev não contém a validação de coerência esperada.",
         call. = FALSE)
  }

  coherence <- parse_choice_answer(
    answers$coherence,
    c("coherent", "absurd"),
    "coerência com o contexto"
  )
  if (identical(coherence$choice, "absurd")) {
    return(list(coherent = FALSE, coherence = coherence))
  }

  if (is.null(answers$difficulty) || is.null(answers$variance)) {
    stop("A resposta coerente do Jev não contém dificuldade e variância.",
         call. = FALSE)
  }

  list(
    coherent = TRUE,
    coherence = coherence,
    difficulty = parse_choice_answer(
      answers$difficulty,
      names(difficulty_mu),
      "dificuldade"
    ),
    variance = parse_choice_answer(
      answers$variance,
      names(variance_sigma),
      "variância"
    )
  )
}

parse_dm_context_review <- function(answers) {
  if (!is.list(answers) ||
      is.null(answers$consistency) ||
      is.null(answers$fundamental_plausibility)) {
    stop("A resposta do Jev não contém as duas validações de contexto esperadas.",
         call. = FALSE)
  }

  consistency <- parse_choice_answer(
    answers$consistency,
    c("consistent", "contradiction"),
    "contradição com o contexto"
  )
  fundamental_plausibility <- parse_choice_answer(
    answers$fundamental_plausibility,
    c("plausible", "fundamentally_absurd"),
    "absurdo fundamental"
  )
  list(
    accepted = identical(consistency$choice, "consistent") &&
      identical(fundamental_plausibility$choice, "plausible"),
    consistency = consistency,
    fundamental_plausibility = fundamental_plausibility
  )
}

validate_dm_context_with_jev <- function(new_context, campaign_context) {
  api_key <- trimws(Sys.getenv("TYPESAFE_API_KEY", unset = ""))
  if (!nzchar(api_key)) {
    stop("Configure a variável de ambiente TYPESAFE_API_KEY para usar o Jev.",
         call. = FALSE)
  }

  request_body <- list(
    model = "jev-latest",
    state = paste(
      "CONTEXTO ATUAL DA CAMPANHA:",
      campaign_context,
      "",
      "INFORMAÇÃO NOVA PROPOSTA PARA O CONTEXTO DO DM:",
      new_context,
      sep = "\n"
    ),
    questions = list(
      consistency = list(
        type = "choice",
        instructions = paste(
          "Avalie somente se a informação nova contradiz diretamente algum fato estabelecido no contexto atual da campanha.",
          "Use consistent se não houver contradição explícita, inclusive quando a informação acrescentar um fato novo ainda não conhecido.",
          "Use contradiction somente quando os dois fatos não puderem ser verdadeiros ao mesmo tempo.",
          "Não avalie ações de personagens, dificuldade, variância, conveniência narrativa ou se a informação já era conhecida."
        ),
        criteria = list(
          consistent = "Informação nova compatível com os fatos atuais, ou que os complementa sem conflito.",
          contradiction = "Afirmação que conflita diretamente com um fato estabelecido no contexto atual."
        )
      ),
      fundamental_plausibility = list(
        type = "choice",
        instructions = paste(
          "Avalie somente se a informação é absurda em nível fundamental.",
          "Aceite elementos fantásticos próprios de uma aventura de fantasia e fatos novos que não contradigam o contexto.",
          "Use fundamentally_absurd apenas para algo logicamente impossível ou sem sentido até mesmo como fato de fantasia.",
          "Não rejeite algo apenas por ser raro, extraordinário, mágico, surpreendente ou improvável.",
          "Não classifique dificuldade ou variância."
        ),
        criteria = list(
          plausible = "Informação compreensível e fundamentalmente possível dentro de uma aventura de fantasia.",
          fundamentally_absurd = "Informação sem sentido ou logicamente impossível até mesmo no contexto fantástico."
        )
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
  if (!is.list(body$answers)) {
    stop("A resposta do Jev não contém validações de contexto válidas.",
         call. = FALSE)
  }
  parse_dm_context_review(body$answers)
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
      coherence = list(
        type = "choice",
        instructions = paste(
          "Antes de classificar a ação, avalie se ela é coerente com os fatos estabelecidos no contexto da aventura.",
          "Use coherent quando a ação puder acontecer na cena, inclusive se for criativa, improvável, arriscada ou introduzir uma ideia nova que não contradiga fatos conhecidos.",
          "Use absurd somente quando a ação contradizer diretamente um fato estabelecido ou for logicamente impossível dentro da situação descrita.",
          "Não considere uma ação absurda apenas por ser difícil, ousada, inesperada ou pouco provável.",
          "Se escolher absurd, não classifique dificuldade nem variância."
        ),
        criteria = list(
          coherent = "A ação é compatível com a situação e os fatos já estabelecidos, ou é uma iniciativa nova plausível.",
          absurd = "A ação contradiz diretamente fatos estabelecidos ou é logicamente impossível na cena atual."
        )
      ),
      difficulty = list(
        type = "choice",
        instructions = paste(
          "Se a resposta de coerência for coherent, classifique a dificuldade da ação descrita.",
          "Escolha a opção que melhor representa o desafio para o personagem."
        ),
        criteria = difficulty_criteria
      ),
      variance = list(
        type = "choice",
        instructions = paste(
          "Se a resposta de coerência for coherent, classifique a variância da ação.",
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

  if (!is.list(body$answers)) {
    stop("A resposta do Jev não contém classificações válidas.",
         call. = FALSE)
  }

  parse_jev_answers(body$answers)
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

action_campaign_memory <- function(result, narration_text = NULL) {
  content <- paste(
    paste("Ação tentada:", result$event),
    paste(
      "Desfecho estabelecido:",
      if (result$success) {
        "o objetivo foi alcançado."
      } else {
        "o objetivo não foi alcançado."
      }
    ),
    if (!is.null(narration_text)) {
      paste("Narração da cena:", trimws(narration_text))
    },
    sep = "\n"
  )
  metadata <- list(
    event = result$event,
    difficulty = result$difficulty,
    variance = result$variance,
    mu = result$mu,
    sigma = result$sigma,
    roll = result$roll,
    percentile = result$percentile,
    is_typical = result$is_typical,
    expected_roll = result$expected_roll,
    roll_delta = result$roll_delta,
    expected_sigma_delta = result$expected_sigma_delta,
    sigma_score = result$sigma_score,
    sigma_band_index = result$sigma_band_index,
    sigma_band = result$sigma_band,
    sigma_adjective = result$sigma_adjective,
    success_threshold = result$success_threshold,
    success_threshold_index = result$success_threshold_index,
    success = result$success,
    outcome = result$outcome,
    jev_classification = result$jev_classification,
    context_used = result$campaign_context
  )
  if (!is.null(narration_text)) {
    metadata$narration <- trimws(narration_text)
  }
  list(content = content, metadata = metadata)
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
  request <- httr2::request(paste0(ollama_base_url(), "/api/chat")) |>
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
  tags$script(HTML("
    (function() {
      function attachCampaignStoryScroll() {
        var output = document.getElementById('campaignStory');
        if (!output) return;

        function scrollToEnd() {
          var story = output.querySelector('#campaign-story-scroll');
          if (story) story.scrollTop = story.scrollHeight;
        }

        new MutationObserver(function() {
          window.requestAnimationFrame(scrollToEnd);
        }).observe(output, { childList: true, subtree: true });

        window.requestAnimationFrame(scrollToEnd);
      }

      $(document).one('shiny:connected', attachCampaignStoryScroll);
    })();
  ")),
  fluidRow(
    wellPanel(
      style = "background-color:#494949;",
      width = "100%",
      div(
        style = paste(
          "display:flex;align-items:center;justify-content:center;",
          "position:relative;"
        ),
        h1(
          "Sala de Controle do DM",
          style = "margin:10px 0;font-weight:bold;color:white;"
        ),
        actionButton(
          "showCharacterSheet",
          label = NULL,
          icon = icon("user"),
          title = "Abrir ficha do personagem",
          class = "btn-default",
          style = paste(
            "position:absolute;right:0;",
            "background:transparent;color:white;border-color:white;"
          )
        )
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
      plotOutput("plotDistribution", height = "460px"),
      hr(),
      div(
        style = "display:flex;align-items:center;gap:10px;",
        h4("Narração", style = "margin:0;"),
        actionButton("generateNarrative", "Gerar Narrativa"),
        uiOutput("narrativeStatus", inline = TRUE)
      ),
      br(),
      uiOutput("narration"),
      h4("Resultado da ação"),
      uiOutput("actionResult"),
      hr(),
      h3("Contexto do DM"),
      textAreaInput(
        "dmContext",
        "Informações novas para a campanha",
        value = "",
        rows = 4,
        width = "100%",
        placeholder = "Ex.: A estrada ao norte cruza uma velha ponte de pedra..."
      ),
      actionButton("addDmContext", "Validar e adicionar contexto")
    )
  )
)

server <- function(input, output, session) {
  campaign_id <- trimws(Sys.getenv("RPG_CAMPAIGN_ID", unset = "pedra-clara"))
  if (!nzchar(campaign_id)) {
    stop("RPG_CAMPAIGN_ID não pode ser vazio.", call. = FALSE)
  }
  campaign_store <- open_campaign_store()
  ensure_campaign_seed(campaign_store, campaign_id)

  jev_result <- reactiveVal(NULL)
  action_result <- reactiveVal(NULL)
  narration <- reactiveVal(NULL)
  status_message <- reactiveVal(paste(
    "Memória persistente pronta para a campanha",
    shQuote(campaign_id),
    "— descreva um evento e peça uma análise ao Jev."
  ))
  generation_running <- reactiveVal(FALSE)
  generation_id <- reactiveVal(0L)
  active_generation <- reactiveVal(NULL)
  active_cancel <- reactiveVal(NULL)
  campaign_story_stream <- reactiveVal(NULL)
  narrative_status <- reactiveVal(NULL)
  campaign_context_snapshot <- reactiveVal(paste(
    "CONTEXTO-BASE DA CAMPANHA:",
    campaign_context_seed,
    sep = "\n"
  ))
  campaign_revision <- reactiveVal(0L)
  character_sheet_revision <- reactiveVal(0L)
  character_sheet <- reactiveVal(NULL)
  character_sheet_status <- reactiveVal("Aguardando a leitura da memória da campanha.")
  character_sheet_generation <- reactiveVal(0L)
  character_sheet_cancel <- reactiveVal(NULL)
  character_sheet_dirty <- reactiveVal(FALSE)

  persist_campaign_memory <- function(
    kind,
    content,
    metadata,
    memory_id = NULL,
    refresh_sheet = TRUE
  ) {
    if (is.null(memory_id)) {
      memory_id <- tryCatch(
        insert_campaign_memory(
          campaign_store,
          campaign_id,
          kind,
          content,
          metadata
        ),
        error = function(error) {
          list(
            id = NULL,
            persisted = FALSE,
            error = conditionMessage(error)
          )
        }
      )
      if (is.list(memory_id)) {
        return(memory_id)
      }
    } else {
      update_error <- tryCatch({
        update_campaign_memory(
          campaign_store,
          memory_id,
          content,
          metadata
        )
        NULL
      }, error = function(error) conditionMessage(error))
      if (!is.null(update_error)) {
        return(list(
          id = memory_id,
          persisted = TRUE,
          error = update_error
        ))
      }
    }

    campaign_revision(shiny::isolate(campaign_revision()) + 1L)
    indexing_error <- tryCatch({
      index_pending_campaign_memories(campaign_store, campaign_id)
      NULL
    }, error = function(error) conditionMessage(error))
    if (isTRUE(refresh_sheet)) {
      character_sheet_dirty(FALSE)
      character_sheet_revision(
        shiny::isolate(character_sheet_revision()) + 1L
      )
    } else {
      character_sheet_dirty(TRUE)
    }
    list(
      id = memory_id,
      persisted = TRUE,
      indexing_error = indexing_error
    )
  }

  memory_status <- function(result, success_message) {
    if (!isTRUE(result$persisted)) {
      return(paste(
        success_message,
        "O banco não conseguiu salvar este registro:",
        result$error
      ))
    }
    if (!is.null(result$error)) {
      return(paste(
        success_message,
        "O registro anterior continua salvo, mas não foi possível atualizá-lo:",
        result$error
      ))
    }
    if (!is.null(result$indexing_error)) {
      return(paste(
        success_message,
        "O registro foi salvo, mas a indexação vetorial falhou; será tentada novamente na próxima interação:",
        result$indexing_error
      ))
    }
    success_message
  }

  refresh_character_sheet <- function() {
    previous_cancel <- shiny::isolate(character_sheet_cancel())
    if (is.function(previous_cancel)) {
      previous_cancel()
    }

    request_id <- shiny::isolate(character_sheet_generation()) + 1L
    character_sheet_generation(request_id)
    character_sheet_cancel(NULL)
    character_sheet_status("Atualizando a ficha com a memória da campanha...")

    context <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        paste(
          "FICHA DO PERSONAGEM PRINCIPAL:",
          "nome, classe, ancestralidade e divindade do protagonista;",
          "nomes, descrições e funções de seus companheiros;",
          "nível, pontos de vida, atributos, perícias, habilidades e equipamento confirmados."
        ),
        relevant_limit = 30L,
        recent_limit = 10L
      ),
      error = function(error) {
        character_sheet_status(paste(
          "Não foi possível recuperar a memória vetorial para a ficha:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(context)) {
      return(invisible(NULL))
    }

    prompts <- character_sheet_prompts(context)
    is_current_request <- function() {
      !session$isClosed() &&
        identical(
          shiny::isolate(character_sheet_generation()),
          request_id
        )
    }
    flush_sheet <- function() {
      if (!session$isClosed()) {
        session$flushOutput()
      }
    }

    tryCatch(
      stream_ollama_response(
        system_prompt = prompts$system,
        user_prompt = prompts$user,
        on_token = function(token) invisible(NULL),
        on_complete = function(response_text) {
          if (!is_current_request()) {
            return()
          }
          sheet <- tryCatch(
            parse_character_sheet(response_text),
            error = function(error) error
          )
          if (inherits(sheet, "error")) {
            character_sheet_status(paste(
              "Não foi possível interpretar a ficha retornada pela LLM:",
              conditionMessage(sheet)
            ))
          } else {
            character_sheet(sheet)
            character_sheet_status("Ficha atualizada com as memórias da campanha.")
          }
          character_sheet_cancel(NULL)
          flush_sheet()
        },
        on_error = function(message) {
          if (!is_current_request()) {
            return()
          }
          character_sheet_status(paste(
            "Não foi possível atualizar a ficha com a LLM:",
            message
          ))
          character_sheet_cancel(NULL)
          flush_sheet()
        },
        register_cancel = function(cancel) {
          if (is_current_request()) {
            character_sheet_cancel(cancel)
          } else {
            cancel()
          }
        },
        is_cancelled = function() !is_current_request()
      ),
      error = function(error) {
        if (is_current_request()) {
          character_sheet_status(paste(
            "Não foi possível atualizar a ficha com a LLM:",
            conditionMessage(error)
          ))
          character_sheet_cancel(NULL)
          flush_sheet()
        }
      }
    )
    invisible(NULL)
  }

  refresh_character_sheet_if_dirty <- function() {
    if (!isTRUE(shiny::isolate(character_sheet_dirty()))) {
      return(invisible(NULL))
    }
    character_sheet_dirty(FALSE)
    character_sheet_revision(
      shiny::isolate(character_sheet_revision()) + 1L
    )
    invisible(NULL)
  }

  observeEvent(character_sheet_revision(), {
    refresh_character_sheet()
  }, ignoreInit = FALSE)

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
    narrative_status(NULL)
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
    refresh_character_sheet_if_dirty()
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
    narrative_status("Gerando continuação da aventura...")
    context_snapshot <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        "Continue a cena atual da aventura sem recomeçar a história."
      ),
      error = function(error) {
        status_message(paste(
          "Não foi possível recuperar a memória vetorial da campanha:",
          conditionMessage(error)
        ))
        narrative_status("Falha ao recuperar a memória da campanha.")
        NULL
      }
    )
    if (is.null(context_snapshot)) {
      return()
    }
    campaign_context_snapshot(context_snapshot)
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
        narration(response_text)
        persisted <- persist_campaign_memory(
          "narrative",
          paste("Continuação da aventura:", trimws(response_text)),
          list(
            narration = trimws(response_text),
            context_used = context_snapshot
          )
        )
        campaign_story_stream(NULL)
        message <- memory_status(persisted, "Narrativa atualizada.")
        narrative_status(message)
        status_message(message)
      },
      on_error = function(message) {
        narrative_status(paste("Falha ao gerar narrativa:", message))
        status_message(paste("Não foi possível gerar a narrativa:", message))
      }
    )
  })

  observeEvent(input$showCharacterSheet, {
    showModal(modalDialog(
      title = tagList(icon("user"), "Ficha do personagem"),
      uiOutput("characterSheet"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })

  observeEvent(input$simulateCharacterAction, {
    cancel_active_generation(message = NULL, flush = FALSE)
    context_snapshot <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        "Sugira uma ação possível para um personagem neste momento da aventura."
      ),
      error = function(error) {
        status_message(paste(
          "Não foi possível recuperar a memória vetorial da campanha:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(context_snapshot)) {
      return()
    }
    campaign_context_snapshot(context_snapshot)
    prompts <- character_action_prompts(
      context_snapshot
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

  observeEvent(input$addDmContext, {
    new_context <- trimws(input$dmContext)
    if (!nzchar(new_context)) {
      status_message("Escreva uma informação nova antes de validar o contexto.")
      return()
    }

    status_message("Validando a nova informação de contexto com o Jev...")
    context_snapshot <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        paste("VALIDAÇÃO DE NOVO CONTEXTO:", new_context)
      ),
      error = function(error) {
        status_message(paste(
          "Não foi possível recuperar a memória vetorial da campanha:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(context_snapshot)) {
      return()
    }
    campaign_context_snapshot(context_snapshot)

    review <- tryCatch(
      validate_dm_context_with_jev(new_context, context_snapshot),
      error = function(error) {
        status_message(paste(
          "Não foi possível validar o novo contexto com o Jev:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(review)) {
      return()
    }
    if (!isTRUE(review$accepted)) {
      reasons <- c(
        if (identical(review$consistency$choice, "contradiction")) {
          "a informação contradiz fatos já estabelecidos"
        },
        if (identical(
          review$fundamental_plausibility$choice,
          "fundamentally_absurd"
        )) {
          "a informação foi considerada absurda em nível fundamental"
        }
      )
      status_message(paste0(
        "Contexto não adicionado: ",
        paste(reasons, collapse = " e "),
        ". Revise a informação e tente novamente."
      ))
      return()
    }

    persisted <- persist_campaign_memory(
      "dm_context",
      paste("Informação de contexto do DM:", new_context),
      list(
        information = new_context,
        validation = list(
          consistency = review$consistency,
          fundamental_plausibility = review$fundamental_plausibility
        ),
        context_used = context_snapshot
      )
    )
    if (isTRUE(persisted$persisted)) {
      updateTextAreaInput(session, "dmContext", value = "")
    }
    status_message(memory_status(
      persisted,
      "Contexto do DM validado e adicionado à memória da campanha."
    ))
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
    context_snapshot <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        paste("AÇÃO ATUAL:", event)
      ),
      error = function(error) {
        status_message(paste(
          "Não foi possível recuperar a memória vetorial da campanha:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(context_snapshot)) {
      return()
    }
    campaign_context_snapshot(context_snapshot)
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
    if (!isTRUE(result$coherent)) {
      status_message(paste(
        "O Jev considerou a entrada incoerente ou absurda em relação ao contexto da aventura.",
        "Nada foi executado. Ajuste a descrição para combinar com a cena e tente novamente."
      ))
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

    context_snapshot <- tryCatch(
      retrieve_campaign_context(
        campaign_store,
        campaign_id,
        paste("AÇÃO ATUAL:", analysis$event)
      ),
      error = function(error) {
        status_message(paste(
          "Não foi possível recuperar a memória vetorial da campanha:",
          conditionMessage(error)
        ))
        NULL
      }
    )
    if (is.null(context_snapshot)) {
      return()
    }
    campaign_context_snapshot(context_snapshot)

    action <- sample_action(mu, sigma)
    result <- list(
      event = analysis$event,
      difficulty = unname(difficulty_labels[[input$selectDifficulty]]),
      variance = unname(variance_labels[[input$selectVariance]]),
      campaign_context = context_snapshot,
      jev_classification = analysis$classification,
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
    jev_result(NULL)
    updateTextAreaInput(session, "eventDescription", value = "")
    narration(NULL)
    campaign_story_stream("")
    memory_entry <- action_campaign_memory(result)
    persisted <- persist_campaign_memory(
      "action",
      memory_entry$content,
      memory_entry$metadata,
      refresh_sheet = FALSE
    )
    if (!isTRUE(persisted$persisted)) {
      status_message(memory_status(
        persisted,
        "A rolagem foi calculada, mas a narração não será iniciada."
      ))
      return()
    }
    result$memory_id <- persisted$id
    result$memory_indexing_error <- persisted$indexing_error
    status_message(memory_status(
      persisted,
      "Rolagem exibida no gráfico. Gerando a narração local..."
    ))
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
        memory_entry <- action_campaign_memory(result, response_text)
        persisted <- persist_campaign_memory(
          "action",
          memory_entry$content,
          memory_entry$metadata,
          memory_id = result$memory_id
        )
        campaign_story_stream(NULL)
        refresh_character_sheet_if_dirty()
        status_message(memory_status(persisted, "Ação e narração concluídas."))
      },
      on_error = function(message) {
        refresh_character_sheet_if_dirty()
        status_message(memory_status(persisted, paste(
          "A ação foi calculada, mas a narração falhou:",
          message
        )))
      }
    )
  })

  session$onSessionEnded(function() {
    cancel_active_generation(message = NULL, flush = FALSE)
    sheet_cancel <- shiny::isolate(character_sheet_cancel())
    if (is.function(sheet_cancel)) {
      sheet_cancel()
    }
    if (DBI::dbIsValid(campaign_store)) {
      DBI::dbDisconnect(campaign_store)
    }
  })

  output$statusText <- renderUI(tags$p(status_message()))

  output$narrativeStatus <- renderUI({
    message <- narrative_status()
    if (is.null(message) || !nzchar(message)) {
      return(NULL)
    }
    tags$span(message, style = "margin-left:4px;")
  })

  output$campaignStory <- renderUI({
    campaign_revision()
    completed_entries <- campaign_story_entries(campaign_store, campaign_id)
    current_narration <- campaign_story_stream()
    show_current <- !is.null(current_narration) &&
      nzchar(current_narration) &&
      (length(completed_entries) == 0L ||
         !identical(
           tail(completed_entries, 1L)[[1]]$narration,
           current_narration
         ))

    tags$section(
      h2("Aventura até agora"),
      tags$div(
        id = "campaign-story-scroll",
        style = paste(
          "max-height:320px;overflow-y:auto;padding:12px;",
          "border:1px solid #ddd;border-radius:4px;margin-bottom:16px;"
        ),
        tags$p(campaign_context_seed),
        lapply(completed_entries, function(entry) {
          tags$div(
            style = "margin-bottom:12px;",
            if (!is.null(entry$event) && nzchar(trimws(entry$event))) {
              tags$p(
                tags$strong("Ação do personagem: "),
                entry$event
              )
            },
            if (!is.null(entry$narration) &&
                nzchar(trimws(entry$narration))) {
              tags$p(
                tags$strong(
                  if (identical(entry$kind, "action")) {
                    "O que aconteceu: "
                  } else {
                    "Continuação: "
                  }
                ),
                entry$narration
              )
            }
          )
        }),
        if (show_current) tags$p(current_narration)
      )
    )
  })

  output$characterSheet <- renderUI({
    sheet <- character_sheet()
    status <- character_sheet_status()
    missing_value <- "Não registrado na base da campanha"
    format_value <- function(value) {
      if (is.null(value)) {
        return(missing_value)
      }
      if (is.list(value)) {
        value <- unlist(value, use.names = FALSE)
        if (length(value) == 0L) {
          return(missing_value)
        }
      }
      paste(as.character(value), collapse = ", ")
    }
    sheet_field <- function(label, value) {
      tags$div(
        style = "margin-bottom:8px;",
        tags$strong(paste0(label, ": ")),
        format_value(value)
      )
    }
    sheet_list <- function(label, values) {
      items <- unlist(values %||% list(), use.names = FALSE)
      tags$div(
        style = "margin-bottom:8px;",
        tags$strong(paste0(label, ": ")),
        if (length(items) == 0L) {
          missing_value
        } else {
          tags$ul(lapply(items, tags$li))
        }
      )
    }
    attributes <- if (is.null(sheet)) list() else sheet$attributes
    attribute_labels <- c(
      strength = "Força",
      dexterity = "Destreza",
      constitution = "Constituição",
      intelligence = "Inteligência",
      wisdom = "Sabedoria",
      charisma = "Carisma"
    )

    tagList(
      tags$p(status, class = "text-muted"),
      if (is.null(sheet)) {
        tags$p("A ficha será preenchida assim que a análise da LLM terminar.")
      } else {
        tagList(
            sheet_field("Nome", sheet$name),
            sheet_field("Classe", sheet$class),
            sheet_field("Ancestralidade", sheet$ancestry),
            sheet_field("Antecedentes", sheet$background),
            sheet_field("Divindade", sheet$deity),
            sheet_field("Nível", sheet$level),
            sheet_field("Pontos de vida", sheet$hit_points),
            tags$strong("Atributos"),
            tags$ul(lapply(names(attribute_labels), function(attribute) {
              tags$li(
                paste0(attribute_labels[[attribute]], ": "),
                format_value(attributes[[attribute]])
              )
            })),
            sheet_list("Perícias", sheet$skills),
            sheet_list("Habilidades", sheet$abilities),
            sheet_list("Equipamento", sheet$equipment),
            sheet_list("Companheiros", sheet$companions),
            sheet_list("Outros fatos", sheet$other_facts),
            tags$p(
              "Os dados são gerados pela LLM a partir das memórias da campanha. O que não estiver estabelecido permanece como não registrado."
            )
        )
      }
    )
  })

  output$campaignContext <- renderUI({
    campaign_revision()
    summary <- campaign_memory_summary(campaign_store, campaign_id)
    memories <- campaign_memory_archive(campaign_store, campaign_id)
    archive <- vapply(seq_len(nrow(memories)), function(index) {
      paste(
        sprintf("[%s #%d]", memories$kind[[index]], memories$id[[index]]),
        memories$content[[index]],
        "Metadados:",
        jsonlite::prettify(memories$metadata[[index]]),
        sep = "\n"
      )
    }, character(1))
    tags$details(
      tags$summary("Contexto da aventura"),
      tags$p(sprintf(
        "Campanha: %s | Registros persistidos: %d | Vetores indexados: %d de %d | Modelo: %s",
        campaign_id,
        summary$total[[1]],
        summary$indexed[[1]],
        summary$total[[1]],
        ollama_embedding_model()
      )),
      h4("Contexto recuperado para a interação mais recente"),
      tags$pre(
        campaign_context_snapshot(),
        style = "white-space:pre-wrap;"
      ),
      h4("Arquivo completo da campanha"),
      tags$pre(
        paste(archive, collapse = "\n\n"),
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
        "Coerência com o contexto: confirmada (confiança %.0f%%)",
        classification$coherence$confidence * 100
      )),
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
        breaks = seq(0, 100, by = 20),
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
        legend.text = element_text(size = 7),
        legend.spacing.x = grid::unit(0.35, "cm"),
        legend.box.spacing = grid::unit(0.15, "cm")
      ) +
      guides(fill = guide_legend(
        ncol = 3,
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
