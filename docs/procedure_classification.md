# Procedure Classification: Regular vs Irregular Access

## Irregular-Accessible (Sans Papiers Eligible)
These procedures do NOT require `regular_situation: true`:
- `aes_spouse` - Admission Exceptionnelle Conjoint
- `admission_exceptional_tension` - AES Métiers en Tension
- `vpf_humanitarian` - Raisons Humanitaires
- `vpf_domestic_violence` - Victime Violence Domestique
- `vpf_trafficking_victim` - Victime Traite/Proxénétisme
- `resident_refugee` - Réfugié (OFPRA/CNDA)
- `naturalization_refugee` - Naturalisation Réfugié
- All `nationality_*` procedures (Nationalité par mariage, naissance, etc.)
- All `naturalization_*` procedures
- All `reintegration_*` procedures

## Regular-Only (Require Valid Visa/Title)
These procedures require `regular_situation: true`:
- All `vpf_*` (except those listed above)
- All `salaried_worker_*`
- All `passeport_talent_*`
- All `resident_*` (except refugee)
- All `aps_*`
- `temp_worker`, `entrepreneur`, `multiannual_general`, `visitor`, etc.
