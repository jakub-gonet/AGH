#!/bin/bash

erl -make && erl -pa bin \
                 -run pollution_test test \
                 -run pollution_server_test test \
                 -s init stop