# Dockerfile for fluff Fortran linter
FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    gfortran \
    wget \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install FPM
ARG FPM_VERSION=0.8.2
RUN wget https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64 && \
    chmod +x fpm-${FPM_VERSION}-linux-x86_64 && \
    mv fpm-${FPM_VERSION}-linux-x86_64 /usr/local/bin/fpm

# Set working directory
WORKDIR /app

# Copy source code
COPY . .

# Build fluff
RUN fpm build

# Install fluff binary
RUN cp build/gfortran_*/app/fluff /usr/local/bin/

# Set entrypoint
ENTRYPOINT ["fluff"]
CMD ["--help"]