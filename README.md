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

O app apresenta um campo para descrever a ação. **Analisar evento com Jev** sugere dificuldade e variância, que podem ser ajustadas antes da rolagem. **Rodar ação** sorteia uma rolagem d100 baseada em uma distribuição normal limitada a 0–100. O percentil dessa distribuição recebe uma transformação suave (*smoothstep*), fazendo a curva acumulada perder inclinação nas duas extremidades.

O Jev avalia dificuldade e variância como dimensões independentes. A dificuldade é classificada em 12 níveis, do mais alto ao mais baixo: Impossível (97), Quase impossível (93), Extremamente difícil (89), Muito difícil (85), Difícil (70), Desafiador (60), Médio (50), Moderadamente fácil (40), Fácil (30), Muito fácil (15), Quase trivial (8) e Trivial (3). Dificuldade representa o quão difícil é alcançar o objetivo; variância representa apenas o quanto o resultado é imprevisível por acaso, fatores ocultos ou circunstâncias instáveis. Uma ação pode ser difícil e previsível, ou fácil e imprevisível.

O app não atribui mais resultados binários nem limiares de sucesso/fracasso. Ollama recebe como fatos o evento descrito, as decisões do Jev para dificuldade e variância, os parâmetros numéricos usados, a rolagem, o percentil e a interpretação sigma. A LLM cria a narração considerando esses elementos em conjunto, sem uma consequência predefinida.

O resultado compara a rolagem com o valor esperado `μ`: diferença = rolagem − μ e desvio padronizado `z = (rolagem − μ) / σ`. O adjetivo sigma é atribuído diretamente por esse `z`: Inimaginável (< −5σ), Inacreditável (−5σ a −4σ), Raríssimo (−4σ a −3σ), Escasso (−3σ a −2σ), Incomum (−2σ a −1σ), Corriqueiro (−1σ a 0σ), Habitual (0σ a +1σ), Atípico (+1σ a +2σ), Excecional (+2σ a +3σ), Extraordinário (+3σ a +4σ), Fenomenal (+4σ a +5σ) e Inconcebível (> +5σ). O resultado mostra separadamente o percentil acumulado do modelo d100 e o percentil normal de referência `pnorm(z)`; as bandas do gráfico são posicionadas pelas distâncias `μ ± kσ`. Ollama recebe o centro esperado, a diferença, o desvio e o adjetivo, e deve calibrar o peso narrativo ao quanto o resultado se afastou do esperado. Um resultado comum perto de μ não deve ser narrado como excepcional; desvios maiores devem produzir efeitos proporcionalmente mais marcantes, sem converter isso em rótulo binário de sucesso ou fracasso. O gráfico e a rolagem permanecem limitados a 0–100.

Na interface, a descrição do evento e os controles de dificuldade/variância ficam na coluna esquerda. O gráfico acumulado aparece na coluna direita, com o resultado da rolagem e a narração logo abaixo.

Não salve a chave TypeSafe em arquivos do repositório. A chamada ao Jev requer conexão com a internet; a narração usa o serviço Ollama local.

## Outros projetos

Este repositório também contém exemplos do Shiny, protótipos estatísticos e dashboards em subdiretórios.
