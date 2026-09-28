#!/usr/bin/env python3
"""
Fix boolean default values in OpenAPI schemas.
Converts string boolean defaults (e.g., "true", "false") to actual booleans.
"""

import json
import sys
from pathlib import Path


def fix_boolean_defaults_in_dict(obj):
    """Recursively fix boolean defaults from string to bool."""
    modified = False
    
    if isinstance(obj, dict):
        if obj.get("type") == "boolean" and "default" in obj:
            if isinstance(obj["default"], str):
                if obj["default"].lower() == "true":
                    obj["default"] = True
                    modified = True
                elif obj["default"].lower() == "false":
                    obj["default"] = False
                    modified = True
        
        for value in obj.values():
            if fix_boolean_defaults_in_dict(value):
                modified = True
    
    elif isinstance(obj, list):
        for item in obj:
            if fix_boolean_defaults_in_dict(item):
                modified = True
    
    return modified


def main():
    script_dir = Path(__file__).parent
    
    json_files = sorted(script_dir.glob("*.json"))
    
    if not json_files:
        print("No JSON files found in openapi directory")
        return 0
    
    files_modified = 0
    
    for json_file in json_files:
        try:
            with open(json_file, 'r') as f:
                content = json.load(f)
            
            if fix_boolean_defaults_in_dict(content):
                with open(json_file, 'w') as f:
                    json.dump(content, f, indent=2)
                print(f"✓ Fixed boolean defaults in {json_file.name}")
                files_modified += 1
        
        except json.JSONDecodeError as e:
            print(f"✗ Invalid JSON in {json_file.name}: {e}", file=sys.stderr)
            return 1
        
        except Exception as e:
            print(f"✗ Error processing {json_file.name}: {e}", file=sys.stderr)
            return 1
    
    if files_modified == 0:
        print("No boolean string defaults found")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
