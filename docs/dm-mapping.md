
# SDTM DM Mapping Specification

## Overview

This document describes the mapping of synthetic raw clinical
demographics into an SDTM-like Demographics (DM) dataset.

| Attribute | Value |
|---|---|
| Study ID | STUDY001 |
| Domain | DM |
| Source dataset | raw_dm.csv |
| Target dataset | dm.csv |
| Record structure | One record per subject |
| Data source | Synthetic clinical trial data |
| Implementation | R / sdtmgenerator |
| Mapping version | 0.1.0 |

## Variable Mapping

| Target Variable | Source | Type | Mapping Rule |
|---|---|---|---|
| STUDYID | Pipeline parameter | Character | Assigned from study_id |
| DOMAIN | Constant | Character | DM |
| USUBJID | study_id, site_id, subject_id | Character | Concatenate with hyphens |
| SUBJID | subject_id | Character | Direct mapping |
| SITEID | site_id | Character | Direct mapping |
| RFSTDTC | rfstdtc | Character | Valid YYYY-MM-DD date or missing |
| AGE | age | Numeric | Direct numeric mapping |
| AGEU | Constant | Character | YEARS |
| SEX | sex | Character | Trim and convert to uppercase |
| ARMCD | arm | Character | Placebo = PBO; Active = ACT |
| ARM | arm | Character | Trim source treatment description |

## Study-Specific Treatment Mapping

| Raw Treatment | ARMCD | ARM |
|---|---|---|
| Placebo | PBO | Placebo |
| Active | ACT | Active |

## Data Quality Rules

- Required source columns must be present.
- The source dataset must contain at least one record.
- Subject identifiers must be unique within the study.
- Subject and site identifiers must not be missing or empty.
- Leading zeros in identifiers must be preserved.
- Age must be numeric and non-negative when present.
- SEX must contain M, F, or U.
- Treatment descriptions must exist in the study-specific mapping.
- RFSTDTC must contain a valid YYYY-MM-DD date or be missing.
- The target dataset must contain all implemented DM variables.
- USUBJID must be unique.
- Raw and target record counts must match.

## Traceability Example

Source record:

subject_id = 001
site_id = 101
age = 35
sex = M
arm = Placebo
rfstdtc = 2026-01-01

Generated record:

STUDYID = STUDY001
DOMAIN = DM
USUBJID = STUDY001-101-001
SUBJID = 001
SITEID = 101
RFSTDTC = 2026-01-01
AGE = 35
AGEU = YEARS
SEX = M
ARMCD = PBO
ARM = Placebo

## Execution Artifacts

- outputs/dm.csv
- outputs/dm_validation_report.csv

## Scope and Limitations

This implementation covers a subset of SDTM DM variables
for educational and portfolio purposes.

It does not establish full CDISC SDTM compliance or
regulatory submission readiness.

Additional SDTM variables, controlled terminology,
study metadata, cross-domain consistency checks,
and formal conformance validation will be addressed
in future versions.

All input data are synthetic and do not contain
real patient information.
