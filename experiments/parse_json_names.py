import os
import re

output_dir = "output"
param_list = []

for fname in os.listdir(output_dir):
    match = re.match(r"(\d+)-(\d+)-(.+)\.json$", fname)
    if match:
        param_list.append([match.group(1), match.group(2), match.group(3)])

print(param_list)