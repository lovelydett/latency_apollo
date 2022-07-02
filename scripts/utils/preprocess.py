# Load raw data and rename them
# Yuting Xie
# 2022.7.1

from numpy import save
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
    "PredictionComponent::PredictionEndToEndProc" : "prediction",
}

PERCEPTION = {
    "FusionComponent::Proc" : "fusion",
    "FusionCameraDetectionComponent::OnReceiveImage" : "fusion_camera",
    "RadarDetectionComponent::Proc" : "radar",
    "DetectionComponent::Proc" : "detection",
    "RecognitionComponent::Proc" : "recognition",
}


def load_csv(filename):
    data = pd.read_csv(filename, sep=',')
    return data

def save_csv(data, filename):
    if filename.find(".csv") < 0:
        filename += ".csv"
    data.to_csv(filename, index=False)

def check(data, taskname):
    return len(data.loc[data["component"] == taskname]) > 0

def isolate_perception(data):
    task_to_data = {}
    for component, taskname in PERCEPTION.items():
        task_to_data[taskname] = data.loc[data["component"] == component]
    return task_to_data

def rename_all(dataset_name):
    dir = "../../data/" + dataset_name
    for root, dirs, files in os.walk(dir):
        for file in files:
            if file.replace(".csv", '') in TASKS.values():
                continue
            data = load_csv(os.path.join(root, file))
            for taskname, out_file in TASKS.items():
                if check(data, taskname):
                    save_csv(data, os.path.join(root, out_file))
                    # Isolate perception results here
                    if taskname == "FusionComponent::Proc":
                        task_to_data = isolate_perception(data)
                        for out_task, sub_data in task_to_data.items():
                            save_csv(sub_data, os.path.join(root, out_task))
                    break


if __name__ == "__main__":
    rename_all("dataset1")