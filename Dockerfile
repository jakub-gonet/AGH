FROM debian
RUN apt-get update && \
    apt-get install -y gdb && \
    apt-get install -y valgrind
CMD ["/bin/bash"]