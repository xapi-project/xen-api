#!/usr/bin/env python3

import json

xenapi_json = "data/xenapi.json"
classes_root = "content/xen-api/classes/"

def template(c):
    return f"""+++
title = "{c}"
layout = "class"
type = "xenapi"
class = "{c}"
+++
"""

def classes():
    with open(xenapi_json) as f:
        xenapi = json.load(f)
    return [c['name'] for c in xenapi]

for c in classes():
    with open(f"{classes_root}{c.lower()}.md", 'w') as f:
        f.write(template(c))
