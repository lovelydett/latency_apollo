# Load raw data and rename them
# Yuting Xie
# 2022.7.1

import pandas as pd
import os

os.chdir("/home/tt/Codes/latency_apollo/scripts/utils/")

TASKS = {
    "RTKLocalizationComponent::Proc" : "localization",
    "FusionComponent::Proc" : "perception",
    "TrafficLightsPerceptionComponent::OnReceiveImage" : "trafficlight",
    "PlanningComponent::Proc" : "planning",
    "LaneDetectionComponent::OnReceiveImage" : "lane",
    "ControlComponent::Proc" : "control",
}


def load_csv(filename):
    data = pd.read_csv(filename, sep=',')
    return data

def check(data, taskname):
    return len(data.loc[data["component"] == taskname]) > 0

def rename_all(dataset_name):
    dir = "../../data/" + dataset_name
    for root, dirs, files in os.walk(dir):
        for file in files:
            data = load_csv(os.path.join(root, file))
            for taskname, out_name in TASKS.items():
                if check(data, taskname):
                    data.to_csv(os.path.join(root, out_name + ".csv"))
                    break

if __name__ == "__main__":
    rename_all("dataset2_driving_info")