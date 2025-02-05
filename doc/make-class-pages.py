#!/usr/bin/env python3

import json

xenapi_json = "data/xenapi.json"
classes_root = "content/xen-api/classes/"

releases_yml = "data/releases.yml"
releases_root = "content/xen-api/releases/"

def class_template(c):
    return f"""+++
title = "{c}"
class = "{c}"
+++
"""

def classes():
    with open(xenapi_json, encoding="utf-8") as f_classes:
        xenapi = json.load(f_classes)
    return [c['name'] for c in xenapi]

for cls in classes():
    with open(f"{classes_root}{cls.lower()}.md", 'w', encoding="utf-8") as f:
        f.write(class_template(cls))

def release_template(a, b, w):
    return f"""+++
title = "{b}"
release = "{a}"
weight = {w}
+++
"""

def releases():
    with open(releases_yml, encoding="utf-8") as f_releases:
        return [l.strip().split(': ') for l in f_releases.readlines()]

for weight, (code_name, title) in enumerate(releases()):
    with open(f"{releases_root}{code_name.lower()}.md", 'w', encoding="utf-8") as f:
        f.write(release_template(code_name, title, weight+1))
