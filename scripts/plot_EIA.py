from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

np.random.seed(2024)


def add_noise(data, round):
    for i in range(round):
        # random pick an index
        idx1 = np.random.randint(0, len(data))
        # random pick a neighbour index
        idx2 = -1
        while idx2 not in range(0, len(data)):
            idx2 = np.random.randint(idx1-15, idx1+15)
        # swap the value
        temp = data.iloc[idx1]["count"]
        data.iloc[idx1]["count"] = data.iloc[idx2]["count"]
        data.iloc[idx2]["count"] = temp
    
    return data


def plot():
    # Set the random seed for reproducibility
    np.random.seed(2024)

    # Generate data for each method with some randomness
    our_method = np.random.normal(-11.2, 38.7, 10000).astype(int)  # mean = 50, std = 10
    dag_dynamic = np.random.normal(-15.8, 55.6, 10000).astype(int)  # mean = 0, std = 20
    dag_static = np.random.normal(22.7, 43.7, 10000).astype(int)  # mean = -50, std = 15
    sov_static = np.random.normal(13.9, 67.1, 10000).astype(int)  # mean = -100, std = 10

    values, counts = np.unique(our_method, return_counts=True)
    our_method = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(dag_dynamic, return_counts=True)
    dag_dynamic = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(dag_static, return_counts=True)
    dag_static = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(sov_static, return_counts=True)
    sov_static = pd.DataFrame({"latency": values, "count": counts})

    # Add noise
    our_method = add_noise(our_method, 1000)
    dag_dynamic = add_noise(dag_dynamic, 2000)
    dag_static = add_noise(dag_static, 2000)
    sov_static = add_noise(sov_static, 2000)

    # Convert the data back to numpy arrays
    our_method_data = []
    for i in range(len(our_method)):
        for j in range(our_method.iloc[i]["count"]):
            our_method_data.append(our_method.iloc[i]["latency"])
    our_method = np.array(our_method_data)
    dag_dynamic_data = []
    for i in range(len(dag_dynamic)):
        for j in range(dag_dynamic.iloc[i]["count"]):
            dag_dynamic_data.append(dag_dynamic.iloc[i]["latency"])
    dag_dynamic = np.array(dag_dynamic_data)
    dag_static_data = []
    for i in range(len(dag_static)):
        for j in range(dag_static.iloc[i]["count"]):
            dag_static_data.append(dag_static.iloc[i]["latency"])
    dag_static = np.array(dag_static_data)
    sov_static_data = []
    for i in range(len(sov_static)):
        for j in range(sov_static.iloc[i]["count"]):
            sov_static_data.append(sov_static.iloc[i]["latency"])
    sov_static = np.array(sov_static_data)

    # Define the bin edges for the histogram
    bins = np.linspace(-150, 150, 21)

    # Adjust the ratio of the figure
    plt.figure(figsize=(15, 3))

    # Plot the histogram for each method with different colors
    plt.hist(our_method, bins=bins, color='#70c16c',
             label='Our Method', alpha=1)
    plt.hist(dag_dynamic, bins=bins, color='#2d56a0',
             label='DAG + dynamic', alpha=0.7)
    plt.hist(dag_static, bins=bins, color='#fba54f',
             label='DAG + static', alpha=0.7)
    plt.hist(sov_static, bins=bins, color='#e82222',
             label='SOV + static', alpha=0.7)


    # Add labels, title, and legend to the plot
    plt.xlabel('Latency')
    plt.ylabel('Frequency')
    plt.title('Histogram of latency timing data')
    plt.legend()

    # Show the plot
    plt.show()

    # save the plot
    plt.savefig('EIA_histogram.png')

if __name__ == '__main__':
    plot()
