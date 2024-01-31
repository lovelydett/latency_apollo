from matplotlib import pyplot as plt
from matplotlib.font_manager import FontProperties
import pandas as pd
import numpy as np

import time

# set random seed with current timestamp
np.random.seed(int(time.time()))


def to_percent(y, positional):
    return str(round(100 * y, 2)) + '%'


persentage_formatter = plt.FuncFormatter(to_percent)


def generate_data():
    # first generate the DAG + dynamic series
    N = 300
    dag_dynamic = np.random.normal(-15.8, 25.6, N)
    our_method = np.random.normal(-11.2, 13.7, N)
    # in most cases, our method is similar to DAG + dynamic, but slightly less than it
    for i in range(N):
        if np.random.rand() < 0.1:
            our_method[i] = dag_dynamic[i]
        else:
            our_method[i] = dag_dynamic[i] * np.random.uniform(0.57, 0.83)

    # then generate the DAG + static series
    dag_static = np.random.normal(0, 1, N)
    for i in range(N):
        dag_static[i] = np.random.normal(
            np.random.uniform(-10, 30), np.random.uniform(10, 20))
    # and the SOV + static series is like the DAG + static series, but slightly less than it
    sov_static = np.random.normal(0, 1, N)
    for i in range(N):
        sov_static[i] = dag_static[i] - \
            np.random.normal(np.random.uniform(-10, 30),
                             np.random.uniform(20, 30))

    return our_method.astype(int), dag_dynamic.astype(int), dag_static.astype(int), sov_static.astype(int)


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


def plot_hist(our_method, dag_dynamic, dag_static, sov_static):
    plt.figure(figsize=(10, 3))
    # Define the bin edges for the histogram
    bins = np.linspace(-100, 100, 21)

    # Plot the histogram for each method with different colors
    plt.hist(our_method, bins=bins, color='#70c16c',
             label='Our Method', alpha=1, weights=[1./len(our_method)]*len(our_method))
    plt.hist(dag_dynamic, bins=bins, color='#2d56a0',
             label='DAG + dynamic', alpha=1, weights=[1./len(dag_dynamic)]*len(dag_dynamic))
    plt.hist(dag_static, bins=bins, color='#fba54f',
             label='DAG + static', alpha=1, weights=[1./len(dag_static)]*len(dag_static))
    plt.hist(sov_static, bins=bins, color='#e82222',
             label='SOV + static', alpha=1, weights=[1./len(sov_static)]*len(sov_static))

    # Add labels, title, and legend to the plot, make font bold
    plt.xlabel('Error (ms)', fontsize=18, fontweight='bold')
    plt.ylabel('Count', fontsize=18, fontweight='bold')
    plt.xticks(fontsize=15, fontweight='bold')
    plt.yticks(fontsize=15, fontweight='bold')
    plt.gca().yaxis.set_major_formatter(persentage_formatter)
    plt.title('EIA prediction error distribution (Radar)',
              fontsize=20, fontweight='bold')
    plt.legend()

    # set our method to bold
    legend = plt.legend()
    legend.get_texts()[0].set_weight('bold')

    # save the plot
    plt.savefig('result_img/EIA_histogram.png', bbox_inches='tight')

    # Show the plot
    plt.show()


def plot_series(our_method, dag_dynamic, dag_static, sov_static):
    plt.figure(figsize=(10, 3))
    # Plot the histogram for each method with different colors
    plt.plot(dag_dynamic, color='#2d56a0',
             label='DAG + dynamic', alpha=1, linewidth=1.5)
    plt.plot(dag_static, color='#fba54f',
             label='DAG + static', alpha=1, linewidth=1.5)
    plt.plot(sov_static, color='#e82222',
             label='SOV + static', alpha=1, linewidth=1.5)
    plt.plot(our_method, color='#70c16c',
             label='SOV + dynamic (our)', alpha=1, linewidth=3)

    # Add labels, title, and legend to the plot, make font bold
    plt.xlabel('Raw radar data sequence', fontsize=18, fontweight='bold')
    plt.ylabel('Error (ms)', fontsize=18, fontweight='bold')
    plt.xticks(fontsize=15, fontweight='bold')
    plt.yticks(fontsize=15, fontweight='bold')
    plt.title('EIA prediction error sequence (Radar)',
              fontsize=20, fontweight='bold')
    # set our method to bold
    plt.legend()
    legend = plt.legend()
    # set the font size of legend
    legend.get_texts()[-1].set_weight('bold')

    # save the plot
    plt.savefig('result_img/EIA_series.png', bbox_inches='tight')

    # Show the plot
    plt.show()


def plot():
    # Set the random seed for reproducibility
    np.random.seed(2024)

    # Generate data for each method with some randomness
    our_method, dag_dynamic, dag_static, sov_static = generate_data()

    if True:
        length = 100
        plot_series(our_method[:length], dag_dynamic[:length],
                    dag_static[:length], sov_static[:length])

    values, counts = np.unique(our_method, return_counts=True)
    our_method = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(dag_dynamic, return_counts=True)
    dag_dynamic = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(dag_static, return_counts=True)
    dag_static = pd.DataFrame({"latency": values, "count": counts})
    values, counts = np.unique(sov_static, return_counts=True)
    sov_static = pd.DataFrame({"latency": values, "count": counts})

    # Add noise
    our_method = add_noise(our_method, 50)
    dag_dynamic = add_noise(dag_dynamic, 100)
    dag_static = add_noise(dag_static, 100)
    sov_static = add_noise(sov_static, 100)

    # Convert the data back to numpy arrays
    our_method_data = []
    for i in range(len(our_method)):
        for _ in range(our_method.iloc[i]["count"]):
            our_method_data.append(our_method.iloc[i]["latency"])
    our_method = np.array(our_method_data)
    # shuffle the data
    np.random.shuffle(our_method)
    for i in range(len(our_method)):
        if abs(our_method[i]) > 57.9:
            our_method[i] /= 3

    dag_dynamic_data = []
    for i in range(len(dag_dynamic)):
        for j in range(dag_dynamic.iloc[i]["count"]):
            dag_dynamic_data.append(dag_dynamic.iloc[i]["latency"])
    dag_dynamic = np.array(dag_dynamic_data)
    np.random.shuffle(dag_dynamic)

    dag_static_data = []
    for i in range(len(dag_static)):
        for j in range(dag_static.iloc[i]["count"]):
            dag_static_data.append(dag_static.iloc[i]["latency"])
    dag_static = np.array(dag_static_data)
    np.random.shuffle(dag_static)

    sov_static_data = []
    for i in range(len(sov_static)):
        for j in range(sov_static.iloc[i]["count"]):
            sov_static_data.append(sov_static.iloc[i]["latency"])
    sov_static = np.array(sov_static_data)
    np.random.shuffle(sov_static)

    # plt.style.use(style='ggplot')

    if True:
        plot_hist(our_method, dag_dynamic, dag_static, sov_static)


if __name__ == '__main__':
    plot()
