# Projetos em R

Protótipos e desenvolvimento em R, incluindo a aplicação de RPG estatisticamente coerente em `Rrpg/RRpgT`.

## Executar o app de RPG

O app usa R, Shiny, ggplot2 e httr2. Instale os pacotes necessários uma vez:

```r
install.packages(c("shiny", "ggplot2", "httr2"))
```

O Jev classifica o evento pela API TypeSafe. Obtenha uma chave na [console TypeSafe](https://console.typesafe.ai/keys) e crie um arquivo local `Rrpg/RRpgT/.env` a partir do exemplo:

```sh
cp Rrpg/RRpgT/.env.example Rrpg/RRpgT/.env
```

Edite `.env` e preencha `TYPESAFE_API_KEY=` com a chave. O app carrega esse arquivo ao iniciar, e o Git o ignora; não cole nem versione a chave. Também é possível definir a variável de ambiente `TYPESAFE_API_KEY` fora do app.

O Ollama precisa estar em execução no endereço local padrão `http://127.0.0.1:11434`, com o modelo `gemma4:latest` disponível. Confira com `ollama list`; se necessário, obtenha o modelo com `ollama pull gemma4:latest`.

Inicie o app a partir do diretório do projeto:

```sh
cd Rrpg/RRpgT
Rscript -e 'shiny::runApp(".")'
```

O app apresenta um campo para descrever a ação. **Analisar evento com Jev** sugere dificuldade e variância, que podem ser ajustadas antes da rolagem. **Rodar ação** sorteia um percentil uniforme com `runif()` e o transforma pela inversa da distribuição normal truncada em 0–100 e suavizada por *smoothstep*. Assim, a curva acumulada é sigmoidal, com caudas mais suaves, e varia conforme dificuldade e variância.

O Jev avalia dificuldade e variância como dimensões independentes. A dificuldade é classificada em 12 níveis, do mais alto ao mais baixo: Impossível (97), Quase impossível (93), Extremamente difícil (89), Muito difícil (85), Difícil (70), Desafiador (60), Médio (50), Moderadamente fácil (40), Fácil (30), Muito fácil (15), Quase trivial (8) e Trivial (3). Dificuldade representa o quão difícil é alcançar o objetivo; variância representa apenas o quanto o resultado é imprevisível por acaso, fatores ocultos ou circunstâncias instáveis. Uma ação pode ser difícil e previsível, ou fácil e imprevisível.

O app determina o veredito mecânico como **SUCESSO** se a faixa observada for igual ou superior ao limite da dificuldade na escala de raridade; abaixo dele, é **FALHA**. Esse veredito aparece em destaque em **Resultado da ação**. Ollama recebe o contexto da aventura, a situação atual e se o objetivo foi alcançado, como orientação interna. A LLM atua como Mestre de Jogo (DM) em roleplay, encenando o que acontece com linguagem coloquial, teatral e concisa; quando couber, inclui falas curtas em discurso direto. A resposta não deve avaliar as capacidades do personagem, usar classificações como “Corriqueiro”, nem mencionar mecânicas, regras ou rótulos de sucesso/falha: deve mostrar o desfecho somente pelas ações e reações da cena.

Cada sessão começa com um contexto-base: o grupo chega à vila de Pedra Clara, onde Mara, dona da estalagem, pede ajuda para encontrar Raul e a caravana desaparecida na mata. A abertura e as narrações já concluídas aparecem em **Aventura até agora**, antes do campo **Descreva o evento**; a narração em andamento também é mostrada ali em tempo real. Depois do campo de ação fica **Analisar evento com Jev**, seguido pelo botão **Simular ação de personagem**, que pede ao Ollama uma intenção em primeira pessoa coerente com a cena e preenche o campo; a ação só é executada quando o usuário a revisa e inicia o fluxo normal de análise e rolagem. Ao lado do título **Narração**, **Gerar Narrativa** pede ao DM para continuar a cena sem realizar uma rolagem, e a continuação concluída passa a fazer parte da memória da aventura. Depois de cada narração concluída, o app guarda a ação e o que foi narrado; nas próximas interações, envia esse histórico junto da nova ação no campo `state` do Jev e no contexto do Ollama. A memória mantém até os oito acontecimentos mais recentes, além do contexto inicial, e pode ser consultada em **Contexto da aventura**. Essa memória é mantida durante a sessão do Shiny; ao iniciar uma sessão nova, ela volta ao contexto-base.

O resultado informa duas medidas distintas. A distância em relação ao centro esperado é `(rolagem − μ) / σ`; ela descreve quanto e em que direção o valor rolado se afastou de `μ`. A raridade é calculada a partir do percentil acumulado da curva suavizada: `z_raridade = qnorm(percentil)`. É essa segunda medida que determina os 12 adjetivos probabilísticos: Inimaginável (< −5σ), Inacreditável (−5σ a −4σ), Raríssimo (−4σ a −3σ), Escasso (−3σ a −2σ), Incomum (−2σ a −1σ), Corriqueiro (−1σ a 0σ), Habitual (0σ a +1σ), Atípico (+1σ a +2σ), Excecional (+2σ a +3σ), Extraordinário (+3σ a +4σ), Fenomenal (+4σ a +5σ) e Inconcebível (> +5σ). O app aplica os limites de sucesso por dificuldade: Impossível exige Inconcebível; Quase impossível exige Fenomenal; Extremamente difícil exige Extraordinário; Muito difícil exige Excecional; Difícil exige Atípico; Desafiador exige Habitual; Médio exige Corriqueiro; Moderadamente fácil exige Incomum; Fácil exige Escasso; Muito fácil exige Raríssimo; Quase trivial exige Inacreditável; Trivial exige Inimaginável. O resultado precisa estar na faixa mínima ou acima para ser sucesso; abaixo dela é fracasso. Assim, uma tarefa quase impossível só tem sucesso com resultado Fenomenal ou Inconcebível. A indicação “Na faixa típica” corresponde às classes entre −1σ e +1σ, que cobrem aproximadamente 68,27% dos percentis. No gráfico, as 12 faixas coloridas são verticais e ocupam toda a altura, com limites no eixo da rolagem obtidos invertendo a curva nos percentis sigma correspondentes. A posição e largura dessas mesmas faixas acompanham a dificuldade e a variância para que o marcador esteja sempre na faixa do adjetivo sigma apresentado; as classes, cores e a ordem vermelho-para-azul permanecem iguais. Esses detalhes estatísticos são usados pelo app para determinar o desfecho, mas não são enviados à LLM: a narração usa apenas a situação e se o objetivo foi alcançado, apresentando as consequências dentro da ficção. A rolagem é apresentada com até 15 casas decimais, sem arredondamento forçado a duas casas, e o gráfico permanece limitado a 0–100.

Na interface, a descrição do evento e os controles de dificuldade/variância ficam na coluna esquerda. O resumo da sugestão do Jev fica recolhido e pode ser aberto ao clicar em **Sugestão do Jev**. O gráfico mostra a curva acumulada sigmoidal e o marcador do resultado sobre 12 faixas verticais de raridade; a legenda exibe as classes sem título para dar mais espaço aos itens. A curva se move e se alarga/estreita com dificuldade e variância; as faixas mantêm as mesmas classes, cores e ordem, acompanhando a escala horizontal para conservar o alinhamento entre o marcador e o adjetivo sigma. Depois do sorteio, aparece **Gerando resposta do DM...** enquanto o pedido de narração é processado pelo Ollama. A narração aparece em streaming, conforme os trechos de texto chegam, e o resultado detalhado da rolagem fica abaixo dela. **Rodar ação** permanece disponível durante a geração; um novo clique cancela a narração em curso e inicia uma nova rolagem e narração. O botão **Cancelar** também permite interromper a geração e manter o resultado da rolagem.

Não salve a chave TypeSafe em arquivos do repositório. A chamada ao Jev requer conexão com a internet; a narração usa o serviço Ollama local.

## Outros projetos

Este repositório também contém exemplos do Shiny, protótipos estatísticos e dashboards em subdiretórios.
