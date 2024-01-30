from matplotlib import pyplot as plt
import pandas as pd
import numpy as np


def prepare_data():
    # we need four kinds of data frames
    # 1. Our method
    data_our = pd.DataFrame(np.random.normal(
        -11.3, 87, size=(100000, 1)), columns=['data'])

    # 2. DAG + dynamic
    data_dag_dynamic = pd.DataFrame(np.random.normal(
        -13.6, 127, size=(100000, 1), columns=['data'])

    # 3. DAG + static
    data = pd.DataFrame(np.random.normal(
        0, 100, size=(100000, 1)), columns=['data'])
    # conert to integers
    data['data'] = data['data'].astype(int)
    return data


def plot():
    # plot the histogram of the data
    data = prepare_data()
    plt.hist(data['data'], bins=100)
    plt.xlabel('error (ms)')
    plt.ylabel('count')
    plt.title('Histogram of Error')
    plt.show()


if __name__ == '__main__':
    plot()
