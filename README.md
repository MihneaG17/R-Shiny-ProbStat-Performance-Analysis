<div align="center">
  <h1>📊 Online Service Performance: Probabilistic Analysis</h1>
  <p>An interactive R Shiny dashboard simulating and analyzing the probabilistic behavior, performance limits, and economic impact of an online server architecture.</p>
</div>

---

## 📌 Short Description
Developed for the **Probability and Statistics** course, this interactive web application uses Monte Carlo simulations to model the behavior of an online service under various traffic conditions. It bridges the gap between theoretical statistics and real-world software engineering by analyzing server response times, timeout rates, Service Level Agreements (SLAs) breaches, user churn, and their direct impact on business profitability.

## 💻 Tech Stack
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge)

* **Language:** R
* **Framework:** Shiny (Interactive Web Apps)
* **UI/Theming:** `bslib` (Bootswatch "Pulse" theme)
* **Core Mechanisms:** Monte Carlo Simulations, Random Number Generation (`rgamma`, `rpois`, `rbinom`, `rnorm`)

## ✨ Mathematical & Engineering Concepts Explored

This dashboard is divided into 13 comprehensive analytical modules. Here are the core concepts implemented:

| Module / Concept | Application in the Simulator | Mathematical Models Used |
| :--- | :--- | :--- |
| **Traffic Modeling** | Simulates daily server requests, contrasting infinite potential traffic vs. a capped market size. | Poisson Distribution ($\lambda$), Binomial Distribution ($n, p$) |
| **Latency & Response** | Models server processing times. Compares asymmetric real-world latencies to symmetric models.  | Gamma Distribution (Shape, Scale), Normal Distribution ($\mu, \sigma^2$) |
| **Joint Variables** | Analyzes the dependency between the number of retry attempts ($N$) and the number of failures ($F$) or total time ($T$). | Pearson Correlation, Linear Regression, $\chi^2$ Test of Independence |
| **Conditional Logic** | Evaluates the user experience by analyzing success rates under specific SLAs. | Conditional Probability $P(A|B)$, Conditional Expectations $E[T|I=1]$ |
| **Probabilistic Bounds** | Validates simulation limits and worst-case scenarios for server overloads using theoretical inequalities. | Markov, Chebyshev, Chernoff, and Jensen Bounds |
| **Central Limit Theorem** | Aggregates latency over months to show how total processing time converges to a normal curve. | TLC / Normal Approximation |
| **Economic Impact** | Calculates net profit by weighing successful requests against SLA penalties and technical user churn. | Expected Value, Profit Optimization Formulas |

## 🚀 Setup & Execution

To run this dashboard locally, you need [R](https://cran.r-project.org/) and optionally [RStudio](https://posit.co/download/rstudio-desktop/) installed on your machine.

**1. Clone the repository:**
```bash
git clone https://github.com/MihneaG17/R-Shiny-ProbStat-Performance-Analysis.git
cd R-Shiny-ProbStat-Performance-Analysis
```

**2. Install required R packages:**

Open your R console or RStudio and run:
```bash
install.packages(c("shiny", "bslib"))
```

**3. Run the application:**

You can start the Shiny app directly from the console:
```bash
shiny::runApp("app.R")
```
(The dashboard will open automatically in your default web browser).
