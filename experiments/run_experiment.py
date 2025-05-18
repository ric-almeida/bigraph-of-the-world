import subprocess
import csv
import re
import os
import types
import sys

# Parameters for the experiment
experiment_type = types.SimpleNamespace()
experiment_type.BUILD = 0
experiment_type.LOAD = 1
experiment_type.ONE_REACTION = 2
experiment_type.ALL_REACTIONS = 3

bigraph_type = types.SimpleNamespace()
bigraph_type.ORIGINAL = 0
bigraph_type.IMPROVED = 1

experiment = experiment_type.ALL_REACTIONS  # Experiment to run
bigraph = bigraph_type.IMPROVED
repeats = 100  # Number of times to repeat each experiment

param_list = [
    # ["10", "2604777", "Dover"],
    # ["8", "295349", "Fenland"],
    # ["8", "295352", "East Cambridgeshire"],
    # ["8", "295351", "Huntingdonshire"],
    # ["8", "295355", "Cambridge"],
    # ["8", "295353", "South Cambridgeshire"],
    ["6", "180837", "Cambridgeshire"],
    # ["8", "172987", "Liverpool"],
    # ["6", "1906767", "Glasgow City"],
    # ["8", "146656", "Manchester"],
    # ["8", "2305279", "Carlsbad"],
    # ["8", "2305280", "Oceanside"],
    # ["8", "111848", "Chula Vista"],
    # ["2", "536780", "Singapore"],
    # ["8", "253832", "San Diego"],
    # ["8", "162378", "Birmingham"],
]

if experiment == experiment_type.BUILD:
    if bigraph == bigraph_type.ORIGINAL:
        executable = ["/usr/bin/time", "-v", "experiments/botw_BitIntSet.exe"]
        output_csv = "experiments/experiment_results_build_BitIntSet.csv"
    else:
        executable = ["/usr/bin/time", "-v", "experiments/botw_improved_trans.exe"]
        output_csv = "experiments/experiment_results_build.csv"
elif experiment == experiment_type.LOAD:
    executable = ["/usr/bin/time", "-v", "experiments/botw_improved_trans.exe"]
    output_csv = "experiments/experiment_results_load.csv"
elif experiment == experiment_type.ONE_REACTION:
    if bigraph == bigraph_type.ORIGINAL:
        executable = ["/usr/bin/time", "-v", "experiments/botw_naive_trans.exe"]
        output_csv = "experiments/experiment_results_naive_trans.csv"
    else:
        executable = ["/usr/bin/time", "-v", "experiments/botw_improved_trans.exe"]
        output_csv = "experiments/experiment_results_dfs_trans.csv"
elif experiment == experiment_type.ALL_REACTIONS:
    executable = ["/usr/bin/time", "-v", "experiments/botw_improved_trans.exe"]
    output_csv = "experiments/experiment_results_reactions.csv"

arguments = ["-eval"]
match experiment:
    case experiment_type.BUILD:
        arguments += ["-write-json"]
    case experiment_type.LOAD:
        arguments += []
    case experiment_type.ONE_REACTION:
        arguments += ["-one-reaction"]
    case experiment_type.ALL_REACTIONS:
        arguments += ["-all-reactions"]

# Metrics to extract
metrics = [
    "Number of open links",
    "Bigraph built in",
    "Bigraph loaded in",
    "Number of nodes",
    "Number of edges",
    "Number of boundaries",
    "Number of streets",
    "Number of buildings",
    "Number of junctions",
    "Added agent in",
    "leave_building",
    "move_across_linked_streets",
    "enter_building starting in street",
    "enter_building_from_street",
    "leave_street",
    "enter_building_from_boundary",
    "enter_building starting in boundary",
    "enter_street",
    "Maximum resident set size (kbytes)",
    "Number of outer names",
]

param_headers = ["admin_level", "id", "name"]
fieldnames = param_headers + metrics

file_exists = os.path.isfile(output_csv)

for repeat in range(repeats):
    for params in param_list:
        sys.stdout.write("Runnning experiment %d on parameters %s       \r" % ((repeat+1), str(params)) )
        sys.stdout.flush()
        with open(output_csv, "a", newline="") as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            if not file_exists:
                writer.writeheader()
                file_exists = True
            while True:
                result = subprocess.run(executable + params + arguments, capture_output=True, text=True)
                output = result.stdout + result.stderr
                data = {k: "" for k in metrics}
                for line in output.splitlines():
                    for metric in metrics:
                        if line.strip().startswith(metric+":"):
                            match = re.search(r"([-+]?\d*\.\d+|\d+)", line)
                            if match:
                                if metric == "Number of open links":
                                    data[metric]+= match.group(0) +";"
                                else:
                                    data[metric] = match.group(0)
                # Add parameter values to the row
                if (experiment==experiment_type.BUILD and data["Bigraph built in"] == "") or (experiment!=experiment_type.BUILD and data["Bigraph loaded in"] == ""):
                    raise Exception("Could not build or load bigraph!")
                if experiment==experiment_type.ONE_REACTION and data["leave_building"] == "":
                    raise Exception("Could not apply leave_building reaction!")
                if experiment==experiment_type.ALL_REACTIONS and data["enter_street"] == "":
                    print("Retrying experiment " + str(repeat+1) + ",running reactions on " + str(params))
                    continue
                row = dict(zip(param_headers, params))
                row.update(data)
                writer.writerow(row)
                break

print(f"Results written to {output_csv}        ")