#!/usr/bin/env python3
"""
Script to add 'regular_situation' criteria to all procedures that require it.
Irregular-accessible procedures are exempt.
"""

import json

# Procedures that do NOT require regular_situation (irregular-accessible)
IRREGULAR_ACCESSIBLE = {
    "aes_spouse",
    "admission_exceptional_tension",
    "vpf_humanitarian",
    "vpf_domestic_violence",
    "vpf_trafficking_victim",
    "resident_refugee",
    "naturalization_refugee",
    "naturalization_decree",
    "naturalization_exceptional",
    "nationality_marriage",
    "nationality_born_france",
    "nationality_ascendant",
    "nationality_sibling",
    "nationality_adopted",
    "nationality_recueilli",
    "reintegration_declaration",
    "reintegration_decree",
}

REGULAR_SITUATION_CRITERION = {
    "id": "regular_situation",
    "label": "Situation administrative régulière (Visa ou Titre)",
    "type": "bool",
    "expected": True
}

def main():
    with open("data/eligibility_criteria.json", "r", encoding="utf-8") as f:
        data = json.load(f)
    
    modified_count = 0
    skipped_count = 0
    
    for proc in data["procedures"]:
        proc_id = proc["id"]
        
        # Skip irregular-accessible procedures
        if proc_id in IRREGULAR_ACCESSIBLE:
            print(f"SKIP (irregular-accessible): {proc_id}")
            skipped_count += 1
            continue
        
        # Check if regular_situation already exists
        existing_ids = [c["id"] for c in proc["base_criteria"]]
        if "regular_situation" in existing_ids:
            print(f"SKIP (already has): {proc_id}")
            skipped_count += 1
            continue
        
        # Add regular_situation as FIRST criterion
        proc["base_criteria"].insert(0, REGULAR_SITUATION_CRITERION.copy())
        print(f"ADDED: {proc_id}")
        modified_count += 1
    
    # Write back
    with open("data/eligibility_criteria.json", "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    
    print(f"\n=== SUMMARY ===")
    print(f"Modified: {modified_count}")
    print(f"Skipped: {skipped_count}")

if __name__ == "__main__":
    main()
