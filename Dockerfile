FROM debian
RUN apt-get update && \
    apt-get install -y gdb valgrind build-essential man-db manpages coreutils less time
CMD ["/bin/bash"]
