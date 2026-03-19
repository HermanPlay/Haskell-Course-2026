# Haskell Course — Solutions Repository

This repository contains my solutions for the Haskell course homeworks (2026). Each assignment is implemented as a standalone solution and organized according to the course guidelines so it is easy to load, inspect, and test the individual exercises.

## Repository layout

Top-level (relevant) items:

- `homeworks/` — directory containing homework folders:
  - `homeworks/hw1/` — solutions for Homework 1 (example)
    - `Solution.hs` — the implementation file for that homework
  - `homeworks/hw2/`, `homeworks/hw3/`, ... — subsequent homework folders (one per assignment)

- `project/README.md` — this file (overview and usage notes)

Note: each homework folder contains the Haskell source files for that assignment. File names follow the course guideline convention (e.g. `Solution.hs`).

## How to use

You can load an individual homework solution into GHCi to try functions interactively.

Example:
1. Start GHCi in the repository root:
   ```
   ghci
   ```
2. Load a solution file:
   ```
   :load homeworks/hw1/Solution.hs
   ```
3. Call functions defined in the file at the `ghci>` prompt:
   ```
   ghci> someFunction args
   ```

## Structure & conventions

- Each homework is kept in its own folder `homeworks/hwN/`.
- The main solution implementation for an assignment is in `Solution.hs`.
- Functions are written in plain Haskell source (compatible with recent GHC versions).
