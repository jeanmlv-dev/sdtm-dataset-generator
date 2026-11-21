
# Laboratory Test Results (LB) Mapping

## Overview

This document describes the mapping of synthetic raw laboratory
data into an SDTM-like LB dataset.

The implementation is intended for educational and demonstration
purposes and does not establish full CDISC SDTM compliance.

## Source

- File: `inst/extdata/raw_lb.csv`
- Reference domain: DM
- Grain: One record per subject, visit, and laboratory test

## Variable Mapping

| Target | Source / Derivation |
|---|---|
| STUDYID | Derived from DM |
| DOMAIN | Constant "LB" |
| USUBJID | Matched against DM using site_id and subject_id |
| LBSEQ | Sequential record number within each subject |
| LBTESTCD | Mapped from lab_test |
| LBTEST | Descriptive laboratory test name |
| LBORRES | Original result as character |
| LBORRESU | Original measurement unit |
| LBSTRESC | Standardized result as character |
| LBSTRESN | Standardized numeric result |
| LBSTRESU | Standardized measurement unit |
| LBORNRLO | Original reference range lower limit |
| LBORNRHI | Original reference range upper limit |
| LBNRIND | Normal range indicator |
| LBDTC | Collection date in ISO 8601 format |
| LBDY | Study day derived from DM.RFSTDTC |
| VISIT | Visit name |
| VISITNUM | Numeric visit identifier |

## Normal Range Indicator

For numeric results with valid reference limits:

- LOW: Result below the lower limit
- NORMAL: Result within the reference range
- HIGH: Result above the upper limit

The initial implementation uses reference limits supplied in the
synthetic source data.

## Referential Integrity

Every LB subject must exist in DM.

The combination of USUBJID and LBSEQ must be unique.

## Study Day

Study days are derived relative to DM.RFSTDTC.

- On or after the reference date: difference + 1
- Before the reference date: difference
- Missing reference date: missing study day

## Scope

The initial version supports six synthetic laboratory tests
with predefined units and numeric results.

It does not implement official laboratory coding, all possible
unit conversions, or full CDISC SDTM compliance.
