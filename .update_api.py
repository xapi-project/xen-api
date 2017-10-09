from __future__ import print_function
import itertools

if __name__ != "__main__":
    import sys
    sys.exit("This should never be imported as a library")

# Replace module Ref, drop module Legacy and remove Stdext from aPI.ml
with open('lib/aPI.ml', 'r') as api_f:
    api_lines = api_f.readlines()

with open('.ref_replacement', 'r') as ref_f:
    ref_replacement = ref_f.readlines()[:-1]  # 'end\n' is already in the tail


api_head = list(
    itertools.takewhile(lambda line: "module Ref" not in line,
                        api_lines)
)

api_tail = itertools.takewhile(
    lambda line: "module Legacy" not in line,
    itertools.dropwhile(lambda line: not line.startswith('end'),
                        api_lines[len(api_head):])
)

api = itertools.ifilter(
    lambda line: "Stdext" not in line,
    itertools.chain(api_head, ref_replacement, api_tail)
)

with open('lib/aPI.ml', 'w') as api_f:
    api_f.writelines(api)
    api_f.flush()

# Drop unneeded stdext from event_types.ml

with open('lib/event_types.ml', 'r') as event_f:
    event_lines = event_f.readlines()

event = itertools.ifilter(
    lambda line: "Stdext" not in line,
    event_lines
)

with open('lib/event_types.ml', 'w') as event_f:
    event_f.writelines(event)
    event_f.flush()

# Update Ref in event_helper
with open('lib/event_helper.ml', 'r') as event_f:
    event_lines = event_f.readlines()

event_head = list(
    itertools.takewhile(
        lambda line: line.strip() != "",
        event_lines
    ))

event = itertools.chain(event_head,
                        "module Ref = API.Ref",
                        event_lines[len(event_head):]
                        )

with open('lib/event_helper.ml', 'w') as event_f:
    event_f.writelines(event)
    event_f.flush()

