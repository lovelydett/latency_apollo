# Analyse the timing results recorded from Baidu Apollo
# Yuting Xie
# 2022.6.29

# Main tasks:
# 0. Data preparation
# 1. Comparative
#   1.1 ANOVA (or other time series comparison methods) applied on whole/solo results.
#   1.2 Derive some satistics to reveal the degree of contention.
# 2. Driving scenarios
#   2.1 Test the correlation between important component-specified information and latency.
#   2.2 Test the correlation between up-stream/down-stream tasks.
# 3. Communication Overheads
#   3.1 Connect the messages with the timestamp and compute the E2E latency difference between components.
#   3.2 Create the scheduling graph with the timestamp results.
# 4. Time series modeling
#   4.1 Stationary test for each component series.
#   4.2 White noise test for each component series.
#   4.3 
