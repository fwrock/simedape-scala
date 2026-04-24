# SimEDaPE REST API

Base URL: `http://localhost:9000`

All request and response bodies are JSON (`Content-Type: application/json`).

---

## Índice

- [GET /health](#get-health)
- [GET /api/v1/pipeline/status](#get-apiv1pipelinestatus)
- [GET /api/v1/pipeline/progress](#get-apiv1pipelineprogress)
- [POST /api/v1/pipeline/start](#post-apiv1pipelinestart)
- [POST /api/v1/pipeline/cancel](#post-apiv1pipelinecancel)
- [GET /api/v1/config](#get-apiv1config)
- [PATCH /api/v1/config](#patch-apiv1config)
- [GET /api/v1/results/clustering](#get-apiv1resultsclustering)
- [GET /api/v1/results/estimation](#get-apiv1resultsestimation)
- [GET /api/v1/results/validation](#get-apiv1resultsvalidation)
- [GET /api/v1/series/definition](#get-apiv1seriesdefinition)
- [Tipos enumerados](#tipos-enumerados)
- [Definição do tamanho das séries temporais](#definição-do-tamanho-das-séries-temporais)
- [Códigos de estado HTTP](#códigos-de-estado-http)

---

## GET /health

Verifica se o serviço está ativo.

**Resposta 200**
```json
{
  "status": "ok",
  "version": "1.0.0"
}
```

---

## GET /api/v1/pipeline/status

Retorna o estado atual do pipeline (idle, running, completed, failed).

**Resposta 200**
```json
{
  "jobId": "a3f1bc2d",
  "state": "completed",
  "startedAt": "2024-01-15T10:30:00Z",
  "completedAt": "2024-01-15T12:45:00Z",
  "error": null
}
```

---

## GET /api/v1/pipeline/progress

Retorna o progresso detalhado de cada etapa do pipeline, incluindo sub-progresso para etapas paralelas (DTW, Estimation).

**Resposta 200**
```json
{
  "jobId": "a3f1bc2d",
  "state": "running",
  "currentPhase": "WarpingPaths",
  "stages": [
    {
      "phase": "LoadBase",
      "status": "completed",
      "startedAt": "2024-01-15T10:30:00Z",
      "completedAt": "2024-01-15T10:31:20Z",
      "durationMs": 80000,
      "subProgressDone": null,
      "subProgressTotal": null,
      "details": "210220 series"
    },
    {
      "phase": "WarpingPaths",
      "status": "running",
      "startedAt": "2024-01-15T10:40:00Z",
      "completedAt": null,
      "durationMs": null,
      "subProgressDone": 42,
      "subProgressTotal": 64,
      "details": null
    },
    {
      "phase": "LoadPartial",
      "status": "pending",
      "startedAt": null,
      "completedAt": null,
      "durationMs": null,
      "subProgressDone": null,
      "subProgressTotal": null,
      "details": null
    }
  ]
}
```

**Campos de `stages[*]`**

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `phase` | string | Nome da fase (`LoadBase`, `PreprocessBase`, `Cluster`, `WarpingPaths`, `LoadPartial`, `PreprocessPartial`, `Estimation`, `Write`) |
| `status` | string | `pending` / `running` / `completed` / `failed` |
| `startedAt` | string? | ISO-8601 timestamp de início |
| `completedAt` | string? | ISO-8601 timestamp de conclusão |
| `durationMs` | number? | Duração em milissegundos (só disponível quando `status=completed`) |
| `subProgressDone` | number? | Unidades concluídas (ex: clusters com DTW calculado) |
| `subProgressTotal` | number? | Total de unidades (ex: número total de clusters) |
| `details` | string? | Informação adicional da fase |

---

## POST /api/v1/pipeline/start

Inicia uma execução do pipeline. Todos os campos são opcionais; omitir usa o valor configurado em `application.conf`.

**Corpo da requisição**
```json
{
  "baseSimulationId":      "toulouse_toulouse_hybrid_1pct",
  "partialSimulationId":   "toulouse_toulouse_hybrid_5pct",
  "outputSimulationId":    "toulouse_estimated_output",
  "nClusters":             64,
  "estimationMethod":      "Interpolation",
  "percentSample":         1.0,
  "workerParallelism":     8,
  "validationMode":        "Disabled",
  "validationReferenceId": "",
  "dataSource":            "clickhouse",
  "dataDir":               null,
  "startFromPhase":        null,
  "stopAfterPhase":        null,
  "timeWindowStart":       6,
  "timeWindowEnd":         22
}
```

**Campos `startFromPhase` / `stopAfterPhase`**

Permitem execução por fase individual ou retomada de checkpoints.

| Valor | Requisito | Descrição |
|-------|-----------|-----------|
| `"Cluster"` | checkpoint de preprocessamento | Pula carga e preprocessamento |
| `"WarpingPaths"` | checkpoints de preprocessamento + clusterização | Pula até DTW |
| `"LoadPartial"` / `"PreprocessPartial"` / `"Estimation"` | todos os 3 checkpoints anteriores | Retoma da estimação |

Exemplos:
- `"stopAfterPhase": "Cluster"` — Para após clusterização e salva checkpoint
- `"startFromPhase": "WarpingPaths"` — Retoma a partir de checkpoints
- `"startFromPhase": "WarpingPaths", "stopAfterPhase": "WarpingPaths"` — Calcula apenas DTW

**Campos `timeWindowStart` / `timeWindowEnd`**

Filtram as séries temporais por janela horária (horas inteiras, 0–23). Por exemplo `6, 22` inclui apenas intervalos entre 06:00 e 22:00, reduzindo o tamanho das séries de 24h para 16h.

**Resposta 202 Accepted**
```json
{ "jobId": "a3f1bc2d", "state": "running" }
```

**Resposta 409** — pipeline já em execução

---

## POST /api/v1/pipeline/cancel

Cancela o pipeline em execução (melhor esforço).

**Resposta 200**
```json
{ "jobId": "a3f1bc2d", "accepted": true }
```

**Resposta 404** — nenhum pipeline em execução

---

## GET /api/v1/config

Retorna a configuração padrão atual.

**Resposta 200**
```json
{
  "baseSimulationId":      "toulouse_toulouse_hybrid_1pct",
  "partialSimulationId":   "toulouse_toulouse_hybrid_5pct",
  "outputSimulationId":    "toulouse_estimated_output",
  "nClusters":             64,
  "kshapeMaxIter":         100,
  "kshapeNInit":           10,
  "estimationMethod":      "Interpolation",
  "percentSample":         1.0,
  "dtwWindowFraction":     0.1,
  "pcaComponents":         10,
  "dataSource":            "clickhouse",
  "dataDir":               null,
  "warpingPathsDir":       "/tmp/simedape/warping",
  "checkpointDir":         "/tmp/simedape/checkpoints",
  "seriesLength":          96,
  "intervalSize":          900,
  "workerParallelism":     8,
  "validationMode":        "Disabled",
  "validationReferenceId": "",
  "timeWindowStart":       null,
  "timeWindowEnd":         null
}
```

**`checkpointDir`**: diretório para checkpoints intermediários. Sobrescrito por `SIMEDAPE_CHECKPOINT_DIR`.

---

## PATCH /api/v1/config

Atualiza parcialmente a configuração padrão (afeta apenas execuções futuras). Todos os campos são opcionais.

**Corpo**
```json
{
  "nClusters":       32,
  "checkpointDir":   "/data/checkpoints",
  "timeWindowStart": 7,
  "timeWindowEnd":   19
}
```

**Resposta 200** — retorna a configuração completa atualizada (mesmo formato de `GET /api/v1/config`)

---

## GET /api/v1/results/clustering

Snapshot da clusterização da última execução. Disponível após a fase `Cluster` concluir.

**Resposta 200**
```json
{
  "jobId": "a3f1bc2d",
  "nClusters": 64,
  "totalSeries": 210220,
  "seriesLength": 96,
  "intervalSizeSeconds": 900,
  "clusters": [
    {
      "id": 0,
      "size": 3284,
      "simPointKey": "link_4523_to_4524",
      "centroid": [0.12, 0.15, 0.18]
    }
  ]
}
```

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `id` | number | Índice do cluster (0-based) |
| `size` | number | Número de séries no cluster |
| `simPointKey` | string | Chave da série representativa (sim-point) |
| `centroid` | number[] | Centróide normalizado (comprimento = `seriesLength`) |

**Resposta 404** — clusterização não disponível

---

## GET /api/v1/results/estimation

Métricas de tráfego da última estimação.

**Resposta 200**
```json
{
  "jobId": "a3f1bc2d",
  "simulationId": "toulouse_estimated_output",
  "totalSeriesEstimated": 198450,
  "metrics": {
    "avgSpeedMs":  8.34,
    "occupancy":   0.127,
    "avgVehicles": 3.2,
    "kmTraveled":  142500.0
  }
}
```

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `avgSpeedMs` | number | Velocidade média em m/s |
| `occupancy` | number | Ocupação média (0–1) |
| `avgVehicles` | number | Média de veículos por intervalo |
| `kmTraveled` | number | Total de km percorridos estimados |

**Resposta 404** — estimação não disponível

---

## GET /api/v1/results/validation

Métricas de validação da última execução (disponível apenas com `validationMode != Disabled`).

**Resposta 200**
```json
{
  "jobId": "a3f1bc2d",
  "mode": "AgainstBase",
  "referenceSimulationId": "toulouse_toulouse_hybrid_1pct",
  "nLinks": 198450,
  "meanMAE": 1.24,
  "meanRMSE": 2.01,
  "meanMAPE": 18.3,
  "meanDTW": 3.87,
  "meanPearsonR": 0.91,
  "perLink": [
    {
      "key": "link_4523_to_4524",
      "mae": 0.87,
      "rmse": 1.45,
      "mape": 12.1,
      "dtwDistance": 2.34,
      "pearsonR": 0.95
    }
  ]
}
```

**Resposta 404** — validação não disponível

---

## GET /api/v1/series/definition

Definição da forma das séries temporais com base na configuração atual.

**Resposta 200 (sem janela temporal)**
```json
{
  "seriesLength":         96,
  "intervalSizeSeconds":  900,
  "totalDurationSeconds": 86400,
  "timeWindowStartHour":  null,
  "timeWindowEndHour":    null,
  "description": "Full day: 96 intervals of 900s (24.0h total)"
}
```

**Resposta 200 (com janela temporal)**
```json
{
  "seriesLength":         64,
  "intervalSizeSeconds":  900,
  "totalDurationSeconds": 57600,
  "timeWindowStartHour":  6,
  "timeWindowEndHour":    22,
  "description": "Window 6h–22h: 64 intervals of 900s (16.0h)"
}
```

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `seriesLength` | number | Número de pontos em cada série |
| `intervalSizeSeconds` | number | Duração de cada intervalo em segundos |
| `totalDurationSeconds` | number | `seriesLength × intervalSizeSeconds` |
| `timeWindowStartHour` | number? | Início da janela horária (0–23) |
| `timeWindowEndHour` | number? | Fim da janela horária (0–23) |
| `description` | string | Descrição legível |

---

## Tipos enumerados

### `PipelinePhase`

| Valor | Descrição |
|-------|-----------|
| `LoadBase` | Carrega dados da simulação base |
| `PreprocessBase` | Normaliza e segmenta as séries temporais |
| `Cluster` | Clusterização KShape |
| `WarpingPaths` | Caminhos DTW cluster ↔ centróide |
| `LoadPartial` | Carrega dados da simulação parcial (sim-points) |
| `PreprocessPartial` | Preprocessa a simulação parcial |
| `Estimation` | Estima séries não-simuladas |
| `Write` | Grava resultados no ClickHouse |

### `EstimationMethod`

| Valor | Descrição |
|-------|-----------|
| `Interpolation` | Interpolação por warping DTW (padrão) |
| `PCA` | Estimação baseada em PCA/SVD |

### `ValidationMode`

| Valor | Descrição |
|-------|-----------|
| `Disabled` | Sem validação (padrão) |
| `AgainstBase` | Valida contra a simulação base |
| `AgainstReference` | Valida contra uma simulação de referência separada |

---

## Definição do tamanho das séries temporais

O tamanho das séries é determinado por `seriesLength` e `intervalSize` (segundos).

```
totalDuration = seriesLength × intervalSizeSeconds
```

**Exemplos:**

| `seriesLength` | `intervalSize` (s) | Duração | Descrição |
|---------------|--------------------|---------|-----------|
| 96 | 900 | 24h | Dia completo, intervalos de 15min |
| 48 | 1800 | 24h | Dia completo, intervalos de 30min |
| 288 | 300 | 24h | Dia completo, intervalos de 5min |
| 64 | 900 | 16h | Janela 6h–22h, intervalos de 15min |
| 32 | 900 | 8h | Janela de 8h, intervalos de 15min |

Com `timeWindowStart`/`timeWindowEnd`, apenas intervalos dentro da janela são incluídos. O `seriesLength` deve corresponder ao número de intervalos na janela.

---

## Códigos de estado HTTP

| Código | Descrição |
|--------|-----------|
| 200 | Sucesso |
| 202 | Pipeline iniciado |
| 400 | Requisição inválida |
| 404 | Recurso não encontrado / resultado não disponível |
| 409 | Conflito (pipeline já em execução) |
| 503 | Gateway indisponível |
