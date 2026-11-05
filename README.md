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

O app apresenta um campo para descrever a ação. **Analisar evento com Jev** sugere dificuldade e variância, que podem ser ajustadas antes da rolagem. **Rodar ação** sorteia uma rolagem d100 baseada em uma distribuição normal limitada a 0–100. O percentil dessa distribuição recebe uma transformação suave (*smoothstep*), fazendo a curva acumulada perder inclinação nas duas extremidades sem alterar as probabilidades dos marcos:

- abaixo de 5%: **Crítico**, fracasso catastrófico;
- acima de 95%: **Extremo**, sucesso espetacular;
- entre esses marcos, o percentil mínimo de Sucesso é `6% + 84% × dificuldade / 100`; abaixo dele é **Falha**, e a partir dele é **Sucesso**.

Assim, os limiares ficam entre 6% e 90%, reservando ao menos 5 pontos percentuais de faixa de Sucesso antes do limite de Extremo, mesmo na dificuldade máxima. Por exemplo, dificuldade 97 exige percentil de pelo menos 87,48%; dificuldade 85 exige 77,40%; dificuldade 30 exige 31,20%. A ordem das faixas é Crítico, Falha, Sucesso e Extremo. Crítico permanece abaixo de 5% e Extremo acima de 95%. A rolagem e o eixo X do gráfico nunca saem de 0–100; o eixo Y mostra a frequência acumulada de 0% a 100%. Ollama recebe a dificuldade, o limiar, o percentil e o resultado já calculado; deve narrar uma falha no objetivo para Falha/Crítico e não pode transformar esses resultados em sucesso.

Na interface, a descrição do evento e os controles de dificuldade/variância ficam na coluna esquerda. O gráfico acumulado aparece na coluna direita, com o resultado da rolagem e a narração logo abaixo.

Não salve a chave TypeSafe em arquivos do repositório. A chamada ao Jev requer conexão com a internet; a narração usa o serviço Ollama local.

## Outros projetos

Este repositório também contém exemplos do Shiny, protótipos estatísticos e dashboards em subdiretórios.
