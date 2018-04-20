#!/bin/bash
scavetool export -F "CSV-R" -o "data_regr.csv" results/RegressionTest-*.sca
scavetool export -F "CSV-R" -o "data_uni.csv" results/UniformCQI-*.sca
scavetool export -F "CSV-R" -o "data_uni_bestcqi.csv" results/UniformCQI_bestCQI*.sca
scavetool export -F "CSV-R" -o "data_binom.csv" results/BinomialCQI-*.sca
scavetool export -F "CSV-R" -o "data_binom_bestcqi.csv" results/BinomialCQI_bestCQI*.sca
