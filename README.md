# Event Study -- Masterarbeit Universitaet Basel

Dieses Repository enthaelt den Code und die Dokumentation zur
Masterarbeit:

**„Empirische Untersuchung der Kursbewegungen bei der Bekanntgabe von
Finanzergebnissen: Eine Analyse des Schweizer Aktienmarktes"**\
Autor: Alessio Ennio Minerba

------------------------------------------------------------------------

## 📖 Inhalt

-   **`code/`** -- R-Skripte fuer Datenaufbereitung,
    Event-Study-Berechnungen und statistische Analysen\
-   **`notebooks/`** -- RMarkdown-Notebooks mit Hauptworkflow und
    Zusatzanalysen\
-   **`data/`** -- Beispieldaten geteilt
-   **`results/`** -- Abbildungen und Tabellen wie in der Arbeit
    dargestellt\


------------------------------------------------------------------------

##  Ziel der Studie

Die Arbeit untersucht, wie Finanzergebnisse kurzfristig und
mittelfristig Aktienkurse am Schweizer Markt beeinflussen.\
Im Zentrum steht ein Event-Study-Ansatz mit Fokus auf:\
- Abnormale Renditen (AR, CAR, BHAR)\
- Persistenz von Preisreaktionen\
- Einfluss von Investorentypen (Retail vs.Institutionell)\
- Rolle der medialen Tonalitaet\
- Sektorale Spill-over Effekte

------------------------------------------------------------------------

## ️ Installation

1.  Repository klonen:

    ``` bash
    git clone https://github.com/USERNAME/eventstudy-masterarbeit.git
    cd eventstudy-masterarbeit
    ```

2.  In R die benoetigten Pakete installieren:

    ``` r
    install.packages("renv")
    renv::restore()
    ```

3.  Hauptanalyse starten:

    ``` r
    rmarkdown::render("notebooks/eventstudy_main.Rmd")
    ```

------------------------------------------------------------------------

## Daten

Die empirische Analyse basiert auf Daten von **Refinitiv Eikon** und
**LSEG Datastream**.\
Da diese Daten proprietaer sind, enthaelt das Repository **Dummy-Daten**
mit derselben Struktur, damit der Workflow nachvollziehbar bleibt.

Wer die Analyse mit den Originaldaten reproduzieren moechte, benoetigt:\
- Gewinnankuendigungsdaten (Timestamps, Standardized Unexpected
Earnings)\
- Kursdaten der betroffenen Aktien (adjustiert fuer Splits/Dividenden)\
- Swiss Performance Index (SPI) als Benchmark

------------------------------------------------------------------------

## Reproduzierbarkeit

-   Alle Analysen sind in **R (Version 4.1)** umgesetzt.\
-   Abhaengigkeiten werden mit **renv** verwaltet (`renv.lock`).\


------------------------------------------------------------------------

## Lizenz

Die Inhalte duerfen unter Angabe der Quelle weiterverwendet werden.
