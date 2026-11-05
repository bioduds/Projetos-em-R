# Projetos em R

Protótipos e desenvolvimento em R, incluindo a aplicação de RPG estatisticamente coerente em `Rrpg/RRpgT`.

### Novidades da campanha: a memória atravessa as sessões

As crônicas de Pedra Clara já não se perdem quando a sessão termina. A campanha agora registra suas cenas num arquivo SQLite local e transforma cada lembrança em vetor com o Ollama. A cada conversa, busca o que tem a ver com a situação e recupera também os acontecimentos mais recentes — para que nomes, pistas e escolhas antigas possam voltar à mesa na hora certa.

O DM continua recebendo o fio da história para narrar em roleplay, e o Jev considera o contexto ao analisar uma ação. No painel **Contexto da aventura**, dá para espiar o trecho recuperado e consultar o arquivo completo da campanha. E, quando **Gerar Narrativa** entra em ação, o aviso aparece junto ao botão; ao terminar, a nova cena surge em **Narração**, pronta para os jogadores continuarem.

Tudo isso funciona localmente por padrão: banco e embeddings ficam na máquina, sem mandar as crônicas para um serviço externo. A campanha padrão é `pedra-clara`; os detalhes para trocar campanha, banco ou modelo estão abaixo.

## Executar o app de RPG

O app usa R, Shiny, ggplot2 e httr2. Instale os pacotes necessários uma vez:

```r
install.packages(c("shiny", "ggplot2", "httr2", "DBI", "RSQLite", "jsonlite"))
```

O Jev classifica o evento pela API TypeSafe. Obtenha uma chave na [console TypeSafe](https://console.typesafe.ai/keys) e crie um arquivo local `Rrpg/RRpgT/.env` a partir do exemplo:

```sh
cp Rrpg/RRpgT/.env.example Rrpg/RRpgT/.env
```

Edite `.env` e preencha `TYPESAFE_API_KEY=` com a chave. O app carrega esse arquivo ao iniciar, e o Git o ignora; não cole nem versione a chave. Também é possível definir a variável de ambiente `TYPESAFE_API_KEY` fora do app.

O Ollama precisa estar em execução no endereço local padrão `http://127.0.0.1:11434`, com os modelos de conversa e de embeddings disponíveis. Confira com `ollama list`; se necessário, obtenha-os com:

```sh
ollama pull gemma4:latest
ollama pull nomic-embed-text:latest
```

O modelo de embeddings transforma memórias e consultas em vetores para busca semântica local; os embeddings e a narração ficam no Ollama local. Nas análises, o contexto recuperado também é enviado à API TypeSafe do Jev para classificar a ação, como no fluxo já existente. O endereço do Ollama e o modelo de embeddings podem ser alterados em `Rrpg/RRpgT/.env`.

Inicie o app a partir do diretório do projeto:

```sh
cd Rrpg/RRpgT
Rscript -e 'shiny::runApp(".")'
```

### Antes da rolagem, a cena precisa fazer sentido

Ao clicar em **Analisar evento com Jev**, a proposta passa primeiro por um teste de coerência com a memória da campanha. O Jev distingue uma iniciativa criativa — mesmo ousada, arriscada ou improvável — de algo que contradiz diretamente o que já aconteceu ou é logicamente impossível naquela cena. Só o segundo caso é tratado como absurdo: o app avisa o jogador, não sugere dificuldade ou variância e interrompe o fluxo antes de qualquer rolagem. Uma proposta coerente segue normalmente para a análise de dificuldade e variância, que podem ser ajustadas antes de **Rodar ação**.

Esse botão sorteia um percentil uniforme com `runif()` e o transforma pela inversa da distribuição normal truncada em 0–100 e suavizada por *smoothstep*. Assim, a curva acumulada é sigmoidal, com caudas mais suaves, e varia conforme dificuldade e variância.

### Contexto do DM: acrescente novos fatos à crônica

A caixa **Contexto do DM**, abaixo de **Resultado da ação**, permite registrar fatos, lugares, personagens e outros detalhes novos para a campanha. Antes de salvar, o app envia ao Jev a informação proposta junto com o contexto atual e pede somente duas verificações: se há contradição direta com algo já estabelecido e se a informação é absurda em nível fundamental. Não há classificação de dificuldade ou variância nesse fluxo; ideias fantásticas, surpreendentes ou ainda desconhecidas são aceitas quando não contradizem a história e fazem sentido no universo da aventura. Se passar pelas duas verificações, a informação é gravada no banco vetorial persistente e pode ser recuperada nas próximas interações; se não passar, o app explica o motivo e não a adiciona.

O Jev avalia dificuldade e variância como dimensões independentes. A dificuldade é classificada em 12 níveis, do mais alto ao mais baixo: Impossível (97), Quase impossível (93), Extremamente difícil (89), Muito difícil (85), Difícil (70), Desafiador (60), Médio (50), Moderadamente fácil (40), Fácil (30), Muito fácil (15), Quase trivial (8) e Trivial (3). Dificuldade representa o quão difícil é alcançar o objetivo; variância representa apenas o quanto o resultado é imprevisível por acaso, fatores ocultos ou circunstâncias instáveis. Uma ação pode ser difícil e previsível, ou fácil e imprevisível.

O app determina o veredito mecânico como **SUCESSO** se a faixa observada for igual ou superior ao limite da dificuldade na escala de raridade; abaixo dele, é **FALHA**. Esse veredito aparece em destaque em **Resultado da ação**. Ollama recebe o contexto da aventura, a situação atual e se o objetivo foi alcançado, como orientação interna. A LLM atua como Mestre de Jogo (DM) em roleplay, encenando o que acontece com linguagem coloquial, teatral e concisa; quando couber, inclui falas curtas em discurso direto. A resposta não deve avaliar as capacidades do personagem, usar classificações como “Corriqueiro”, nem mencionar mecânicas, regras ou rótulos de sucesso/falha: deve mostrar o desfecho somente pelas ações e reações da cena.

Cada sessão abre a campanha persistente `pedra-clara` (configurável por `RPG_CAMPAIGN_ID`) e garante que o contexto-base — a chegada a Pedra Clara, o pedido de Mara e o desaparecimento de Raul e da caravana — esteja registrado. A memória é armazenada localmente em SQLite, por padrão em `Rrpg/RRpgT/data/campaign_memory.sqlite`; esse diretório é ignorado pelo Git para que o banco da campanha não seja publicado. Cada ação rodada é salva com sua classificação, parâmetros, resultado mecânico e contexto usado; quando a narração termina, ela é acrescentada ao mesmo registro. **Aventura até agora** mostra cada ação do personagem junto com o que aconteceu, e também inclui continuações geradas por **Gerar Narrativa**. Os vetores ficam no próprio banco junto dos textos e metadados, e o painel **Contexto da aventura** permite consultar o contexto recuperado e o arquivo completo.

Ao abrir ou recarregar a página, **Aventura até agora** rola automaticamente até o fim da crônica, deixando visível o trecho mais recente.

O ícone de personagem no canto superior direito abre **Ficha do personagem** em um modal sobre a tela principal. A ficha é montada e atualizada pela LLM a partir das memórias relevantes recuperadas da base vetorial: identidade, classe, ancestralidade, divindade, atributos, perícias, habilidades, equipamento e companheiros. Ela é gerada ao abrir o app e atualizada depois de cada nova memória persistida. O modelo deve manter como não registrados os dados que não aparecem na campanha; a resposta estruturada é validada antes de ser exibida. A geração usa o Ollama local e, se falhar, o modal informa o erro sem substituir silenciosamente uma ficha válida anterior.

Em cada interação com o Jev ou com o Ollama, o app gera um embedding da consulta, busca memórias semanticamente relevantes por similaridade de cosseno e combina até cinco resultados com os cinco registros mais recentes. O contexto-base é sempre incluído. Assim, referências antigas podem voltar a ser recuperadas sem perder a continuidade imediata da cena; o histórico persistido não é limitado aos oito registros da versão anterior. A busca também atualiza registros que ainda estejam sem vetor ou cujo modelo de embedding tenha sido alterado. As ações sugeridas por **Simular ação de personagem** só entram na história depois de analisadas e rodadas.

O banco e o modelo de embeddings são locais por padrão. Para configurar outra campanha ou local, defina `RPG_CAMPAIGN_ID` e `RPG_VECTOR_DB_PATH`; para mudar o serviço de embeddings/conversa, ajuste `OLLAMA_BASE_URL` e `OLLAMA_EMBEDDING_MODEL`. O `.env.example` contém os valores padrão. Uma memória que existia apenas na memória de uma sessão anterior do Shiny não pode ser recuperada retroativamente; a persistência começa com a inicialização do novo banco e segue para as próximas interações.

O resultado informa duas medidas distintas. A distância em relação ao centro esperado é `(rolagem − μ) / σ`; ela descreve quanto e em que direção o valor rolado se afastou de `μ`. A raridade é calculada a partir do percentil acumulado da curva suavizada: `z_raridade = qnorm(percentil)`. É essa segunda medida que determina os 12 adjetivos probabilísticos: Inimaginável (< −5σ), Inacreditável (−5σ a −4σ), Raríssimo (−4σ a −3σ), Escasso (−3σ a −2σ), Incomum (−2σ a −1σ), Corriqueiro (−1σ a 0σ), Habitual (0σ a +1σ), Atípico (+1σ a +2σ), Excecional (+2σ a +3σ), Extraordinário (+3σ a +4σ), Fenomenal (+4σ a +5σ) e Inconcebível (> +5σ). O app aplica os limites de sucesso por dificuldade: Impossível exige Inconcebível; Quase impossível exige Fenomenal; Extremamente difícil exige Extraordinário; Muito difícil exige Excecional; Difícil exige Atípico; Desafiador exige Habitual; Médio exige Corriqueiro; Moderadamente fácil exige Incomum; Fácil exige Escasso; Muito fácil exige Raríssimo; Quase trivial exige Inacreditável; Trivial exige Inimaginável. O resultado precisa estar na faixa mínima ou acima para ser sucesso; abaixo dela é fracasso. Assim, uma tarefa quase impossível só tem sucesso com resultado Fenomenal ou Inconcebível. A indicação “Na faixa típica” corresponde às classes entre −1σ e +1σ, que cobrem aproximadamente 68,27% dos percentis. No gráfico, as 12 faixas coloridas são verticais e ocupam toda a altura, com limites no eixo da rolagem obtidos invertendo a curva nos percentis sigma correspondentes. A posição e largura dessas mesmas faixas acompanham a dificuldade e a variância para que o marcador esteja sempre na faixa do adjetivo sigma apresentado; as classes, cores e a ordem vermelho-para-azul permanecem iguais. Esses detalhes estatísticos são usados pelo app para determinar o desfecho, mas não são enviados à LLM: a narração usa apenas a situação e se o objetivo foi alcançado, apresentando as consequências dentro da ficção. A rolagem é apresentada com até 15 casas decimais, sem arredondamento forçado a duas casas, e o gráfico permanece limitado a 0–100.

Na interface, a descrição do evento e os controles de dificuldade/variância ficam na coluna esquerda. O resumo da sugestão do Jev fica recolhido e pode ser aberto ao clicar em **Sugestão do Jev**. O gráfico mostra a curva acumulada sigmoidal e o marcador do resultado sobre 12 faixas verticais de raridade; sua altura foi compactada e a legenda distribuída em três colunas, preservando a escala percentual completa de 0–100%. A curva se move e se alarga/estreita com dificuldade e variância; as faixas mantêm as mesmas classes, cores e ordem, acompanhando a escala horizontal para conservar o alinhamento entre o marcador e o adjetivo sigma. Depois do sorteio, aparece **Gerando resposta do DM...** enquanto o pedido de narração é processado pelo Ollama. A narração aparece em streaming, conforme os trechos de texto chegam, e o resultado detalhado da rolagem fica abaixo dela. Ao clicar em **Gerar Narrativa**, o estado **Gerando continuação da aventura...** aparece ao lado do botão; quando termina, **Narrativa atualizada.** aparece ali e o texto gerado também é exibido em **Narração**. **Rodar ação** permanece disponível durante a geração; um novo clique cancela a narração em curso e inicia uma nova rolagem e narração. O botão **Cancelar** também permite interromper a geração e manter o resultado da rolagem.

Não salve a chave TypeSafe em arquivos do repositório. A chamada ao Jev requer conexão com a internet; a narração usa o serviço Ollama local.

## Outros projetos

Este repositório também contém exemplos do Shiny, protótipos estatísticos e dashboards em subdiretórios.
