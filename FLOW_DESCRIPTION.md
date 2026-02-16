# FFX Project: High-Level Flow Description

## Overview for Non-Technical Readers

This repository implements an automated machine learning workflow for predicting horse racing outcomes (specifically for Turkish horse racing data). Think of it as a sophisticated prediction system that learns patterns from historical racing data to make future predictions.

**What it does in simple terms:** The system takes data about horses (like their running speed, performance metrics, and race conditions) and automatically builds mathematical formulas that can predict race outcomes. It's like having a data scientist automatically figure out what factors matter most in predicting which horse will win.

---

## The Core Technology: FFX (Fast Function Extraction)

### What is FFX?

FFX is an automated formula-building algorithm that creates simple, interpretable mathematical equations from data. Unlike complex "black box" machine learning models, FFX produces formulas you can actually read and understand.

**Example:** Instead of just saying "Trust me, this will work," FFX might produce a formula like:
```
Prediction = 2.5 * Speed + 1.3 * (Age^0.5) - 0.8 * Weight
```

This shows exactly how different factors combine to make predictions.

### Key Features of FFX:

1. **Symbolic Regression**: Automatically discovers mathematical relationships in data
2. **Deterministic**: Always produces the same result for the same input (no randomness)
3. **Fast**: Uses efficient algorithms to explore many possible formulas quickly
4. **Pareto Optimal**: Finds the best trade-off between accuracy and simplicity

---

## Project Structure

### Main Components:

1. **FFX.py** (1,000+ lines)
   - The core algorithm implementation
   - Contains the mathematical machinery for building formulas

2. **runffx.py** (370+ lines)
   - Command-line interface for running FFX
   - Provides tools for: testing models, splitting data, analyzing data

3. **R Scripts** (pareto.R, paretoCrossEval.R, analPareto.R)
   - Evaluation and analysis tools
   - Calculate betting scenarios and performance metrics

4. **TCL Scripts** (submit.tcl, submitCross.tcl, submitS1234Cross.tcl)
   - Workflow automation scripts
   - Orchestrate running experiments across multiple data combinations

---

## The Complete Workflow

The project runs three types of experiments in sequence:

### **Phase 1: Simple Regression Models (run1 - run72)**

**Purpose:** Build baseline models by training and testing on the same data split.

**What happens:**
1. **Data Setup**: For each combination of:
   - 6 years of data (different racing seasons)
   - 4 track types (different racing surfaces/distances - "cins/pist")
   - 3 output variables (S, Hiz, Derece - likely Speed, Velocity, and Rank)
   
2. **Model Building**:
   - The `submit.tcl` script creates 72 separate run directories (6 × 4 × 3 = 72)
   - Each directory runs FFX to build predictive models
   - FFX produces a "Pareto front" of models (different complexity vs accuracy trade-offs)

3. **Evaluation**:
   - The `pareto.R` script evaluates each model
   - Creates `tahmin.rda` files containing predictions
   - Tests models using a betting simulation to measure practical value

**Analogy:** This is like practicing for a test using the same questions you'll see on the exam - good for understanding, but not realistic.

---

### **Phase 2: Cross-Validation Runs (xrun1 - xrunXXXX)**

**Purpose:** More realistic evaluation by training on one dataset and testing on different data.

**What happens:**
1. **Cross-Validation Setup**:
   - For each output variable (S, Hiz, Derece)
   - Try all combinations of training data × test data
   - This creates many more runs than Phase 1

2. **Model Selection**:
   - Uses models built in Phase 1 (doesn't rebuild from scratch)
   - The `paretoCrossEval.R` script finds the best model from existing runs
   - Only the **last (most complex)** model from the Pareto front is used

3. **Evaluation**:
   - Tests how well models generalize to different years/conditions
   - Simulates betting scenarios to measure real-world performance

**Analogy:** This is like studying with one set of practice problems, then testing yourself on different problems - a better measure of true understanding.

---

### **Phase 3: S1234 Runs (s1234run1 - s1234run72)**

**Purpose:** Final validation using ALL available test data with special S1234 samples.

**What happens:**
1. **Comprehensive Testing**:
   - Uses a special dataset called "S1234" (likely a curated test set)
   - Tests against "trainAll" dataset (all combined data)
   - 6 train yearly combinations × 4 track types × 3 outputs = 72 runs

2. **Final Evaluation**:
   - The most rigorous test of model performance
   - Uses the single best model to predict all available test cases
   - Produces final performance metrics

**Analogy:** This is like taking the final exam after all your practice - using everything you've learned on completely fresh problems.

---

## Technical Details for CS Experts

### FFX Algorithm Components:

1. **Base Function Generation**:
   - Simple bases: Variables with exponents (x^0.5, x^2, etc.)
   - Nonlinear operations: abs(), log10()
   - Threshold functions: Hinge functions (max(0, x-c), max(0, c-x))
   - Interaction terms: Products of simple bases

2. **Model Building Strategy**:
   ```
   Configurable via approach = [inter, denom, expon, nonlin, thresh]
   - inter: Enable interaction terms (x1 * x2)
   - denom: Enable denominator (rational functions)
   - expon: Enable various exponents
   - nonlin: Enable abs() and log10()
   - thresh: Enable threshold/hinge functions
   ```

3. **Regularization**:
   - Uses Elastic Net (combination of L1/L2 regularization)
   - Pathwise learning: Starts with strong regularization, gradually relaxes
   - Rho = 0.95 (aggressive pruning, closer to LASSO than Ridge)

4. **Model Selection**:
   - Creates Pareto front: Trade-off between complexity and error
   - Multiple models returned, from simplest to most complex
   - NMSE (Normalized Mean Squared Error) as primary metric

### Data Flow:

```
Input Data (.in files, .col files)
    ↓
FFX Algorithm (FFX.py via runffx.py)
    ↓
Pareto Front Models (pareto_front_*.csv)
    ↓
R Evaluation Scripts
    ↓
Performance Metrics (tahmin.rda)
    ↓
Betting Simulation Results
```

### Key Evaluation Metrics:

1. **NMSE**: Normalized Mean Squared Error (error relative to output variance)
2. **Kazanc**: Turkish for "profit" - betting simulation performance
3. **Tahmin**: Turkish for "prediction" - prediction rankings
4. **Model Complexity**: Number of bases (terms) in the formula

### Betting Simulation Logic:

The R scripts simulate a betting scenario:
- Each race (kosId) has multiple horses
- Bet amounts are distributed (e.g., 10, 5, 3, 2)
- Models rank horses by predicted performance
- "Kazanc" (profit) = winnings - bet amount
- Success measured by: correct top pick, total profit, average prediction rank

---

## Output Variables Explained:

Based on the code analysis:

1. **S**: Appears to be a performance metric where higher is better (isDecreasing=TRUE)
2. **Hiz**: Turkish for "speed", higher is better (isDecreasing=TRUE)
3. **Derece**: Turkish for "degree/rank", lower is better (isDecreasing=FALSE)

---

## Key Design Decisions:

1. **Why Pareto Front?**
   - Different users may prefer simpler (interpretable) vs. complex (accurate) models
   - No single "best" model exists - depends on use case

2. **Why Cross-Validation?**
   - Training and testing on same data is misleading
   - Cross-validation reveals true generalization capability

3. **Why Elastic Net?**
   - Automatic feature selection (L1 penalty)
   - Handles correlated variables better than pure LASSO (L2 penalty)
   - Produces sparse, interpretable models

4. **Why Symbolic Regression?**
   - Produces interpretable formulas (not black boxes)
   - Can discover physical laws or domain insights
   - Easier to validate and trust in critical applications

---

## How to Use This System:

### Basic Usage:

1. **Prepare Data**:
   - Input features: CSV file with one row per variable
   - Output values: CSV file with one row of target values
   - Variable names: CSV file with variable names

2. **Split Data** (optional):
   ```bash
   ./runffx.py splitdata inputs.csv outputs.csv
   ```

3. **Build and Test Models**:
   ```bash
   ./runffx.py test train_in.csv train_out.csv test_in.csv test_out.csv varnames.csv
   ```

4. **Review Results**:
   - Check `pareto_front_*.csv` for model options
   - Each row shows: number of bases, test error, formula

### Advanced Usage:

Run the full workflow using TCL scripts:
```bash
tclsh submit.tcl          # Phase 1: Basic runs
tclsh submitCross.tcl     # Phase 2: Cross-validation
tclsh submitS1234Cross.tcl # Phase 3: Final validation
```

---

## Dependencies:

- **Python 2.7**: Runtime for FFX
- **NumPy**: Numerical computing
- **SciPy**: Scientific computing
- **scikit-learn**: Machine learning (Elastic Net implementation)
- **R**: Statistical analysis and evaluation
- **TCL**: Workflow automation

---

## Strengths of This Approach:

1. ✅ **Interpretable**: Produces readable mathematical formulas
2. ✅ **Deterministic**: Reproducible results
3. ✅ **Fast**: Efficient pathwise learning algorithm
4. ✅ **Automated**: Minimal manual feature engineering
5. ✅ **Rigorous**: Multiple validation phases

## Limitations:

1. ⚠️ **Python 2.7**: Uses deprecated Python version
2. ⚠️ **Single Output**: Models one output variable at a time
3. ⚠️ **Linear Combinations**: Limited to additions/multiplications of transformed features
4. ⚠️ **Memory Intensive**: Pathwise learning can use significant memory
5. ⚠️ **Domain Specific**: Code is tailored for horse racing predictions

---

## Summary for Stakeholders:

This project is a sophisticated machine learning pipeline that:
- Takes historical horse racing data
- Automatically discovers mathematical patterns
- Builds predictive formulas that are both accurate and understandable
- Rigorously tests predictions using multiple validation strategies
- Simulates real betting scenarios to measure practical value

The key innovation is the FFX algorithm, which finds the "sweet spot" between accuracy and simplicity, giving you not just predictions, but explanations of *why* those predictions make sense.

---

## Further Reading:

- **FFX Paper**: Trent McConaghy, "FFX: Fast, Scalable, Deterministic Symbolic Regression Technology", Genetic Programming Theory and Practice IX, 2011
- **Project Website**: http://www.trent.st/ffx
- **Contact**: Trent McConaghy (trentmc@solidodesign.com)

---

*Document created: 2026-02-16*
*Purpose: Explain the ffxR codebase to technical and non-technical audiences*
