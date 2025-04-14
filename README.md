# UNH Women's Soccer vs America East Rivals: Defining Style Through Data

## How do we play? How do they play? 
## And where can we get better?

Comparing UNH to every team in America East during the 2024 season to find what makes us different and what will help us win in 2025.

### Methodology

#### Data Collection & Cleaning
- 214 conference matches (America East Women’s Soccer)
- 9 teams across 3 seasons (2022–2024)
- Initial feature set: 101 match-level predictors capturing technical, tactical, and event-based data
- Data tidied and structured in R using tidyverse principles

#### Feature Engineering & Selection
- Features with inconsistent event tagging (e.g., Counterattacks, Smart Passes) or rare occurrence (e.g., Free Kicks, Penalties, Red Cards) were removed
- Features directly tied to match outcome (Goals Scored/Conceded) were also excluded
- Finalized into 3 feature sets:
    - Control (n = 9) → Possession style & tempo metrics
    - Efficiency (n = 20) → Conversion & defending efficiency metrics
    - Volume (n = 43) → Total action counts & event volumes

#### Dimensionality Reduction
- UMAP (Uniform Manifold Approximation and Projection) applied for exploratory visualization of playing styles
- PCA (Principal Component Analysis) used to reduce dimensionality within each feature set while retaining ≥80% cumulative explained variance
    - Control: 5 PCs
    - Efficiency: 10 PCs
    - Volume: 13 PCs

#### Modeling Tournament Qualification
- Response variable: America East Tournament Qualification (Top 6 Teams per season)
- Models trained using team-season means of PCs:
    - Linear Discriminant Analysis (LDA)
    - Support Vector Machine (SVM) with linear kernel
    - Logistic Regression (initially included but later removed due to convergence issues with small data)
- Resampling:
    - v-fold cross-validation (v=7)
    - Evaluation metric: Accuracy

#### Reduced Models for Interpretability
- LDA loadings on LD1 analyzed to identify most impactful PCs (thresholding based on % of maximum absolute loading)
- Optimal reduced models selected balancing classification performance vs model simplicity:
    - Control → 3 PCs
    - Efficiency → 2 PCs
    - Volume → 3 PCs

#### Interpretation & Mapping
- Final PCs mapped back to original features to provide interpretable insights into team playing styles and key performance drivers
- Comparative plots visualized team-season means and variability across PCs
- UNH’s profile analyzed relative to peer teams and prior seasons



