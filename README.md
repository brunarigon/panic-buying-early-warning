# Replication Package — An Explainable Early-Warning System for Disruption-Induced Retail Panic Buying

This repository contains the full implementation (R pipeline, Python data-collection utility, and a rendered reproducibility notebook) for the analysis presented in the manuscript **"Explainable machine learning for characterizing disruption-specific panic buying in retail supply chains"**, submitted to *Computers & Industrial Engineering* (Special Issue: *AI in Logistics — Methods, Case Studies and Applications*).

The code and supporting files reproduce every calculation, table, and figure in the paper. For a user-friendly, end-to-end walkthrough of the pipeline and the main estimations, see the compiled reproducibility notebook:

➡️ **[Reproducibility notebook (HTML)](https://panic-buying-ews.netlify.app/)** — also available as [`index.html`](index.html) / [`index.Rmd`](index.Rmd) in this repository.

> **Peer-review note.** This repository is provided for review. Author, affiliation, and acknowledgement information has been intentionally omitted from all documentation for anonymity.

---

## 📄 What the paper is about

Supply chains face heterogeneous, hard-to-predict disruptions — pandemics, floods, highway blockades — during which **consumer panic buying**, rather than physical supply failure, is often the primary driver of essential-item shortages. This study asks a single, sharply framed question:

> **Do heterogeneous external open-data signals provide valid out-of-sample early warning of disruption-induced panic buying, *beyond the autocorrelation already encoded in a market's own demand history*?**

Grounded in the **Stimulus–Organism–Response (S-O-R)** framework, the analysis builds an explainable machine-learning pipeline (XGBoost + SHAP) that benchmarks three specifications — **External-only** (open-data signals), **AR-only** (four autoregressive lags of the demand anomaly), and **Combined** — under a deliberately leakage-proof evaluation protocol. It is validated on the aggregated municipal supermarket sector of **15 municipalities in Santa Catarina, Brazil (2018–2025)**, a road-freight-dependent, multi-hazard emerging-economy setting spanning COVID-19, recurrent floods, and transport blockades.

**Headline findings:**

- **The evaluation protocol *is* the finding.** Replacing a conventional random 80/20 split with **expanding-window temporal cross-validation** collapses the External-only model's apparent skill (test R² ≈ 0.35 under a random split → **R² ≈ −0.10** out-of-sample), exposing ~0.44 R² units of interpolation inflation on autocorrelated, rare-event data.
- **Timing lives in the demand history.** A parsimonious **autoregressive baseline** on the market's own sales history is the only specification that anticipates the onset of anomalous demand regimes out-of-sample; external signals add negligible predictive lift and no operational value.
- **External signals characterize rather than anticipate.** Via SHAP attribution and **regime-conditional panel Local Projections** (Jordà, 2005), the open-data signals carry *regime-specific* information — e.g., a strong food-price-to-panic amplification under geopolitical freight disruptions — that identifies the *type* of disruption underway once it has begun.
- **A transferable protocol.** The paper packages four methodological safeguards — expanding-window temporal CV, a parsimonious AR benchmark, rare-event metrics anchored on AUC-PR, and baseline-corrected lead-time analysis — as a reusable evaluation protocol for any AI-based forecasting claim on temporal panel data under crisis conditions.

The practical reframing: retail early warning is a **division of labor** — *sales history supplies timing, open data supplies characterization* — best combined in a single decision-support layer rather than a single predictive model.

---

## 🖼️ Visual abstract

<img width="2752" height="1536" alt="Retail_Panic_Buying_Strategy" src="https://github.com/user-attachments/assets/5a9ccd4d-6476-4cd1-b636-e55071b7b61e" />


The infographic above summarizes the paper's argument at a glance. Retailers cannot reliably anticipate panic buying from crises, so the study benchmarks an early-warning system to test whether **external open data** improves early detection over a retailer's **own sales history**. The answer is a **strategic division of labor**: the internal, autoregressive sales-history signal supplies the *timing*, acting as a regime-switching detector that flags when demand is entering an anomalous state (+28.3 pp detection lift), whereas external open-data signals carry no standalone out-of-sample predictive value (R² = −0.096) but *characterize* the type of crisis under way — health, geopolitical, or environmental — to inform the correct response. The panel also foregrounds two methodological cautions: conventional 80/20 splits inflate apparent skill by ~0.44 R² units, and most panic episodes are too brief and local for external feeds to anticipate in real time.

---

## 🗺️ Study region

The early-warning system is validated on **15 municipalities across Santa Catarina, Brazil** (2018–2025), selected for hosting a national (INMET) meteorological station and exceeding 30,000 inhabitants, and spanning the state's distinct mesoregions and flood-exposure profiles:

<img width="1600" height="1072" alt="image" src="https://github.com/user-attachments/assets/9a577a3d-bd3f-4c42-9034-e3688bfcc6fd" />

| Mesoregion | Municipalities |
| --- | --- |
| Greater Florianópolis (*Grande Florianópolis*) | Florianópolis |
| Itajaí Valley (*Vale do Itajaí*) | Indaial, Itajaí |
| Northern Plateau & Northeast (*Planalto Norte e Nordeste*) | Itapoá, Rio Negrinho |
| Southern Region (*Sul*) | Araranguá, Laguna |
| Midwest & Highlands (*Meio Oeste e Serra*) | Caçador, Campos Novos, Curitibanos, Joaçaba, Lages |
| Greater West (*Grande Oeste*) | Chapecó, São Miguel do Oeste, Xanxerê |

🔗 **[Interactive map of the study region](https://www.google.com/maps/d/u/1/embed?mid=1eAJI8-ZOgU5zOIyShVemABdIT0Nm-Tg&ehbc=2E312F&noprof=1)**

---

## 🗂️ Documentation map

The remainder of this README moves from high-level structure to technical detail:

```
*   📁 Repository structure     — how the files and folders are organized
*   ⚡ Quick start              — how to run the pipeline and rebuild the notebook
*   💾 Data requirements        — the proprietary fiscal data (ignored by git) and the open data
*   🌐 Google Trends collection — the standalone Python utility
*   📊 Generated outputs        — a dictionary of the key files the pipeline produces
*   🔁 Reproducibility & environment
```

---

## 📁 Repository structure

```
/
├── Temporal_CV_v10.R              # Canonical, end-to-end analysis pipeline (R)
├── Google_Trends.py               # Standalone Google Trends extraction utility (Python)
├── index.Rmd                      # Source of the reproducibility notebook
├── index.html                     # Rendered reproducibility notebook (GitHub Pages)
├── panic-buying-early-warning.Rproj
├── .gitignore                     # Ignores data/input/ (proprietary fiscal data)
│
└── data/
    ├── input/                     # ⛔ NOT tracked by git — proprietary merged panels (see Data requirements)
    ├── Casos_Covid.xlsx           # ✅ Open epidemiological data (COVID-19 cases, SC)
    ├── Pandemic_severityIndex.csv # ✅ Open pandemic-severity index series
    ├── output/                    # ✅ Deflator, baseline-validation figures, pandemic-wave plots
    └── analysis/                  # ✅ All committed results (metrics, figures, PDFs) — see Generated outputs
        ├── temporal_cv_ar_augmented/   # Three-way regression benchmark (headline R² table)
        ├── classification_three_way/   # Rare-event / early-warning classification metrics
        ├── local_projections/          # Impulse-response functions (pooled & crisis-conditional)
        ├── Shap/                        # SHAP attribution matrices and figures
        ├── episode_anatomy/             # Episode catalog, summary statistics, calendar/Sankey plots
        ├── hyperparam_sensitivity.csv   # Hyperparameter-transfer sensitivity check
        └── PDFs/                        # Publication-ready figure PDFs (Figures 2–8, Appendix B)
```

**Note on `.gitignore`.** Only `data/input/` is ignored. All processed *results* (`data/analysis/`, `data/output/`) are committed, so a reviewer can inspect every table and figure **without access to the proprietary source data and without re-running the pipeline**.

---

## ⚡ Quick start

**Requirements.** R (≥ 4.3) with the packages loaded at the top of `Temporal_CV_v10.R`, chiefly: `data.table`, `fixest`, `xgboost`, `ggplot2`, `lubridate`, `zoo`, `readxl`, `writexl`, `dplyr`, `stringr`, `tidyr`, `janitor`, `sidrar` (IPCA deflator retrieval), plus `iml`/SHAP tooling for the interpretation step. The pipeline was last run under R 4.3.2 on Windows 11.

1. **Read the notebook first.** For a documented, section-by-section explanation of the entire pipeline — from raw fiscal receipts to the PBB-Score, the temporal-CV benchmark, the early-warning evaluation, SHAP, and the Local Projections — open the rendered **[HTML notebook](https://brunarigon.github.io/panic-buying-early-warning/)** (or `index.Rmd`). It is the recommended entry point.

2. **Run the pipeline.** `Temporal_CV_v10.R` is the canonical script and runs top to bottom. It self-organizes its outputs into `data/analysis/…` and `data/output/…`, creating subdirectories as needed.
   - **Without the proprietary data:** the script's opening comment marks the exact point (Section 0) where the fiscal panel is read. **Skip to Section 1** and load the processed modelling panel (`data/input/3_Final_data_With_Time_modification_ADDED_Var.csv`) — every downstream analysis (temporal CV, classification, SHAP, Local Projections, episode anatomy) runs from that file alone. That file is part of the non-shared `data/input/` set (see below).
   - Expensive steps cache their results to `.rds` files, so re-runs are fast once the caches exist.

3. **Rebuild the notebook (optional).** Knit `index.Rmd` to regenerate `index.html`.

---

## 💾 Data requirements
 
Running the full pipeline requires two categories of input data: a **proprietary fiscal panel** (not shared) and **open external data** (either committed here or freely retrievable).
 
### ⛔ Proprietary source data (`data/input/` — ignored by git)
 
The behavioral **Response** variable is built from **daily aggregate supermarket sales revenue**, derived from electronic fiscal-receipt (*Nota Fiscal de Consumidor Eletrônica*) records provided by the **Santa Catarina State Treasury (Secretaria de Estado da Fazenda de Santa Catarina, SEF/SC)**.
 
> **This data is not redistributed in this repository, in compliance with the Santa Catarina State Treasury's data-protection guidelines.** Everything that depends on it lives under `data/input/`, which is listed in `.gitignore`. Requests for access should be directed to the Santa Catarina State Treasury.
 
The fiscal series is merged with the open external signals into a set of progressively enriched daily city-panel files (all under `data/input/`). The one required to reproduce the modelling results is:
 
| File | Role |
| --- | --- |
| `3_Final_data_With_Time_modification_ADDED_Var.csv` | **The processed modelling panel.** Long-format daily city panel used by every analysis after Section 1 (temporal CV, classification, SHAP, Local Projections, episode anatomy). |
| `2_5_Final_data_Before_Time_modification_and_new_variables.xlsx` | Pre-time-transformation merged panel (entry point for a full re-run from Section 0). |
| `Final_data_With_API.rds` | Cached panel after IPCA-deflator retrieval and per-capita normalization. |
| *(other `2_…` / `3_…` intermediates)* | Sequential build stages written by the pipeline. |
 
**Schema of the merged panel.** The panel is keyed by `city` × `date` (15 municipalities × daily, 2018–2025 ≈ 43,800 city-day rows). The **Response** is built from the proprietary fiscal series (`total_sales_value_daily`, normalized per capita by `population_2022`), while the predictors operationalize the seven S-O-R domains (prefixes `S_` = Stimulus, `O_` = Organism).
 
**Variable dictionary (S-O-R operationalization).** The table below documents every variable, its search topic or definition, and its data source. It corresponds to the full variable list in Appendix A.2 of the manuscript. Google Trends topics are given in Brazilian Portuguese (English gloss in parentheses); PCA-aggregated indices bundle several correlated search topics into a single component.

<table>
  <thead>
    <tr>
      <th>S-O-R</th>
      <th>Domain</th>
      <th>Variable</th>
      <th>Description / search topics</th>
      <th>Source</th>
    </tr>
  </thead>
  <tbody>
    <!-- STIMULUS SECTION (10 ROWS) -->
    <tr>
      <td rowspan="10"><b>Stimulus (S)</b></td>
      <td rowspan="4">Environmental</td>
      <td><code>S_precipAcc</code></td>
      <td>3-day accumulated precipitation</td>
      <td><a href="https://bdmep.inmet.gov.br/">INMET / BDMEP</a></td>
    </tr>
    <tr>
      <td><code>S_windGustMax</code></td>
      <td>Daily maximum wind gust</td>
      <td><a href="https://bdmep.inmet.gov.br/">INMET / BDMEP</a></td>
    </tr>
    <tr>
      <td><code>S_climateGT</code></td>
      <td>PCA index of topics: <i>previsão do tempo</i> (weather forecast), <i>defesa civil</i> (civil defense), <i>chuva</i> (rain), <i>inundação</i> (flood)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>S_cycloneGT</code></td>
      <td>Topic: <i>ciclone</i> (cyclone)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td rowspan="4">Health</td>
      <td><code>S_pandemicCumulCasesLog</code></td>
      <td>Cumulative COVID-19 confirmed cases (log)</td>
      <td><a href="https://brasil.io/dataset/covid19/caso_full/">Brasil.IO</a></td>
    </tr>
    <tr>
      <td><code>S_pandemicCumulDeathsLog</code></td>
      <td>Cumulative COVID-19 confirmed deaths (log)</td>
      <td><a href="https://brasil.io/dataset/covid19/caso_full/">Brasil.IO</a></td>
    </tr>
    <tr>
      <td><code>S_pandemicGT</code></td>
      <td>Topic: <i>pandemia</i> (pandemic)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>S_healthGT</code></td>
      <td>Topic: <i>saúde</i> (health)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td rowspan="2">Geopolitical</td>
      <td><code>S_strike</code></td>
      <td>Binary strike-event flag</td>
      <td><a href="https://www.gov.br/abin/pt-br">ABIN</a></td>
    </tr>
    <tr>
      <td><code>S_strikeGT</code></td>
      <td>Topic: <i>greve</i> (strike)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <!-- ORGANISM SECTION (9 ROWS) -->
    <tr>
      <td rowspan="9"><b>Organism (O)</b></td>
      <td rowspan="4">Situational</td>
      <td><code>O_supermarketGT</code></td>
      <td>Topic: <i>supermercado</i> (supermarket)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>O_minimarketGT</code></td>
      <td>Topic: <i>mercearia</i> (minimarket)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>O_rationingGT</code></td>
      <td>Topic: <i>racionamento</i> (rationing)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>O_fearGT</code></td>
      <td>Topic: <i>medo</i> (fear)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td rowspan="2">Social</td>
      <td><code>O_newsCount</code></td>
      <td>Daily count of news headlines reporting shortages...</td>
      <td><a href="https://g1.globo.com">G1 (Grupo Globo)</a></td>
    </tr>
    <tr>
      <td><code>O_mobilDist_X</code></td>
      <td>Daily distribution of population movement...</td>
      <td><a href="https://data.humdata.org/dataset/movement-distribution">Meta Data</a></td>
    </tr>
    <tr>
      <td>Institutional</td>
      <td><code>O_govPolicyGT</code></td>
      <td>PCA index of topics: <i>decreto</i>, <i>quarentena</i>...</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td rowspan="2">Economic</td>
      <td><code>O_inflationGT</code></td>
      <td>Topic: <i>inflação</i> (inflation)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <tr>
      <td><code>O_basicFoodBasketGT</code></td>
      <td>Topic: <i>cesta básica</i> (basic food basket)</td>
      <td><a href="https://trends.google.com/">Google Trends</a></td>
    </tr>
    <!-- RESPONSE SECTION -->
    <tr>
      <td><b>Response (R)</b></td>
      <td>Panic Buying</td>
      <td>Daily supermarket sales revenue</td>
      <td>Total daily municipal sales value of fiscal receipts...</td>
      <td><b>SEF/SC</b> (Proprietary)</td>
    </tr>
  </tbody>
</table>

*The dictionary uses the manuscript's canonical variable names; raw column names in the processed CSV may carry minor spelling variants (e.g., `S_climate_Gtrends`). The IPCA food-at-home index used to deflate the Response is documented separately under Externally retrieved open data below.*
 
### ✅ Committed open data (`data/`)

These small, publicly sourced files are included so the health-domain steps run out of the box:

- `data/Casos_Covid.xlsx` — COVID-19 case series for Santa Catarina.
- `data/Pandemic_severityIndex.csv` — pandemic-severity index series.

### 🔗 Externally retrieved open data

Retrieved at runtime or via the utility below, and reproducible from public sources:

- **IPCA — *Alimentação no domicílio*** (food-at-home) deflator, pulled from **IBGE/SIDRA** by the pipeline (`sidrar`) and cached to `data/output/deflator_alimentacao_domicilio_2018_2025.csv`.
- **Google Trends** search-interest series (see next section).
- **Meta Movement Distribution Maps** (population mobility; available from 2022 onward).
- **G1 / Grupo Globo** news headlines, classified into a daily count of genuine shortage/panic signals.

---

## 🌐 Google Trends collection (`Google_Trends.py`)

`Google_Trends.py` is a standalone Python utility for collecting and treating the Google Trends signals used in the Organism and Stimulus domains. Because Google Trends returns *relative* search interest that is rescaled per request window (each window peaks at 100), isolated windows are not directly comparable. The script implements a **stitch-and-anchor extraction protocol** to render windowed daily series comparable on a common absolute scale:

- Retrieves a **monthly master series** for the full 2018–2025 period (long-run trend) plus overlapping **90-day daily windows** (15-day overlaps).
- **Chains** consecutive windows via a multiplicative stitching factor computed over each overlap, then **re-anchors** the stitched daily series to the monthly master.
- Includes IP-block mitigation (rotating user agents, exponential backoff, resumable `.pkl` backups) for robust long-horizon collection.

Configuration (search term, Google topic code, geography `BR-SC`, date range) is set in the "Control Panel" block near the top of the file. Requires `pandas`, `numpy`, and `pytrends`.

---

## 📊 Generated outputs

The pipeline writes results into `data/analysis/…` (committed). The **canonical outputs** underlying the manuscript's tables and figures are:

| Artefact | Path |
| --- | --- |
| Regression comparison (headline three-way R²) | `data/analysis/temporal_cv_ar_augmented/comparison_summary.csv` |
| Per-fold regression metrics | `data/analysis/temporal_cv_ar_augmented/comparison_fold_metrics.csv` |
| Early-warning / classification comparison | `data/analysis/classification_three_way/comparison_summary.csv` |
| Per-fold classification metrics | `data/analysis/classification_three_way/metrics_per_fold.csv` |
| Hyperparameter-transfer sensitivity | `data/analysis/hyperparam_sensitivity.csv` |
| Local Projection impulse responses (by crisis regime) | `data/analysis/local_projections/lp_irf_by_crisis.csv` |
| Episode catalog | `data/analysis/episode_anatomy/episode_table.csv` |
| SHAP feature importance | `data/analysis/Shap/shap_feature_importance.csv` |
| Publication figure PDFs | `data/analysis/PDFs/Figure_*.pdf` |

---

## 🔁 Reproducibility & environment

Every table and figure in the notebook is generated by `index.Rmd` from the committed result files, so the manuscript's empirical claims can be verified without the proprietary source data. The reference environment was **R 4.3.2** on Windows 11; `sessionInfo()` output is printed at the end of the notebook. Note that a small number of steps depend on live external services (IBGE/SIDRA for the deflator, Google Trends, and Meta mobility data), whose availability may change over time.

---

## 📑 Citation

The manuscript is currently under peer review. If you use this code or data pipeline, please cite the reproducibility notebook and the associated manuscript:

> *[Authors withheld for review]* (2026). *Explainable machine learning for characterizing disruption-specific panic buying in retail supply chains.* Manuscript submitted to *Computers & Industrial Engineering* (Special Issue: AI in Logistics). Reproducibility notebook: `index.html`.

*(Full citation details will be added upon acceptance.)*
