# International Portfolio Optimization under ESG Constraints
*This R Shiny dashboard automates international equity portfolio optimization using Markowitz theory constrained by World Bank ESG metrics.*

---

## 🎯 Overview
This project develops an interactive dashboard to construct and analyze optimal financial portfolios under environmental, social, and governance (ESG) constraints.

**Objectives**
- Optimize international portfolios using Markowitz mean-variance theory
- Integrate renewable energy metrics via the World Bank API
- Visualize efficient frontiers and historical cumulative returns
- Provide interactive global mapping of portfolio weights

---

## 🗄️ Data
- **Source:** Yahoo Finance, World Bank (API v2)
- **Time Period / Size:** Rolling 10-year window, monthly frequency (prices) / 2021 (ESG metrics)
- **Preprocessing:** Log-returns calculation, annualized covariance, forward-fill imputation for missing price data
- **Data Availability:** Publicly available via APIs

---

## 🧠 Methodology
- **Theoretical Approach:** Modern Portfolio Theory (Markowitz Mean-Variance Optimization)
- **Mathematical Framework:** Quadratic programming (minimization of variance under return and ESG constraints)
- **Evaluation Strategy:** Efficient frontier simulation, historical cumulative return backtesting

---

## ⚙️ Features
- **Retrieve Financial Data:** Extract 10-year monthly adjusted closing prices via Yahoo Finance API
- **Fetch ESG Metrics:** Integrate renewable energy consumption data via the World Bank API
- **Optimize Portfolio:** Resolve quadratic programming to minimize risk under return and ESG constraints
- **Simulate Efficient Frontier:** Calculate and display optimal risk-return trade-offs dynamically
- **Visualize Global Weights:** Display optimal portfolio allocations on an interactive choropleth map
- **Analyze Historical Performance:** Plot cumulative historical returns of the optimal portfolio selection

---

## 🧰 Tech Stack
- **Language:** R
- **Data Engineering & Acquisition:** httr, jsonlite, rnaturalearth
- **Numerical Computing & Data Manipulation:** sf
- **Quantitative Finance:** quadprog
- **Data Visualization:** leaflet, plotly, DT
- **Dashboards & Web APIs:** shiny, shinyWidgets, shinyjs

---

## 📦 Installation

```bash
git clone https://github.com/floriancrochet/master-year2-dataviz-rshiny2.git
cd master-year2-dataviz-rshiny2
Rscript -e 'install.packages(c("shiny", "httr", "jsonlite", "sf", "leaflet", "plotly", "DT", "shinyWidgets", "shinyjs", "quadprog", "rnaturalearth"))'
```

---

## 💻 Usage Example

### Reproducing the Analysis / Execution Pipeline
```bash
Rscript -e "shiny::runApp('.')"
```

---

## 📂 Project Structure

```text
master-year2-dataviz-rshiny2/
│
├── www/
│   └── style.css
├── .gitignore
├── LICENSE
├── README.md
├── global.R
├── master-year2-dataviz-rshiny2.Rproj
├── server.R
└── ui.R
```

---

## 📜 License
This project is released under the MIT License.  
© 2026 Florian Crochet

---

## 👤 Author
**Florian Crochet**  
[GitHub Profile](https://github.com/floriancrochet)

---

## 🤝 Acknowledgments
This work was conducted as part of the Dataviz: R-Shiny 2 course, supervised by Fabien Morineau.