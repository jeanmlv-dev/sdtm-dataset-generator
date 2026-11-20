
# VS Domain — Mapping Specification

## Overview

The Vital Signs (VS) module transforms synthetic raw clinical
measurements into an SDTM-like findings dataset.

The implementation demonstrates explicit mapping, unit
standardization, subject linkage, and study-day derivation.

It does not establish full CDISC SDTM compliance.

## Input Data

Source: `inst/extdata/raw_vs.csv`

Required columns:

- subject_id
- site_id
- visit
- visitnum
- test_code
- result
- unit
- assessment_date

## Mapping Rules

| SDTM Variable | Source / Derivation |
|---|---|
| STUDYID | DM.STUDYID |
| DOMAIN | Constant "VS" |
| USUBJID | Linked to DM using SITEID and SUBJID |
| VSSEQ | Sequential record number per subject |
| VSTESTCD | test_code |
| VSTEST | Controlled test description |
| VISIT | visit |
| VISITNUM | visitnum |
| VSORRES | Original measurement represented as character |
| VSORRESU | Original unit |
| VSSTRESC | Standardized result represented as character |
| VSSTRESN | Standardized numeric result |
| VSSTRESU | Standardized unit |
| VSDTC | assessment_date |
| VSSTDY | Study day relative to DM.RFSTDTC |

## Supported Tests

| Test Code | Test Name | Standard Unit |
|---|---|---|
| SYSBP | Systolic Blood Pressure | mmHg |
| DIABP | Diastolic Blood Pressure | mmHg |
| HR | Heart Rate | beats/min |
| TEMP | Temperature | C |
| WEIGHT | Weight | kg |
| HEIGHT | Height | cm |

## Unit Standardization

- SYSBP / DIABP: mmHg
- HR: beats/min
- TEMP: Celsius; Fahrenheit converted to Celsius
- WEIGHT: kg
- HEIGHT: cm

Temperature conversion:

C = (F - 32) * 5 / 9

## Study Day

For assessment dates on or after RFSTDTC:

VSSTDY = VSDTC - RFSTDTC + 1

For assessment dates before RFSTDTC:

VSSTDY = VSDTC - RFSTDTC

Study day zero is not used.

## Data Integrity

- Every VS subject must exist in DM.
- Subject and site identifiers must match DM.
- Test codes and units must be supported.
- Dates must be valid ISO 8601 calendar dates.
- Results must be finite numeric measurements.
- Duplicate source assessments are rejected.
- VSSEQ must be unique within each USUBJID.

## Limitations

This is a synthetic educational implementation.

Test terminology and units are simplified and have not
been verified against an official CDISC controlled
terminology release.

The module does not implement complete SDTM metadata,
Define-XML, or regulatory conformance validation.
