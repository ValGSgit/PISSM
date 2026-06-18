FROM ubuntu:24.04

# Set working directory
WORKDIR /app

# Install build dependencies
RUN apt-get update && apt-get install -y \
    gfortran \
    build-essential \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy the project into the container
COPY . /app

# Build the project
WORKDIR /app/pissm
RUN make release

# Set the executable as the entry point
ENTRYPOINT ["/app/pissm/bin/pissm"]
CMD ["/root"]
