# Docker Setup for PISSM

This guide explains how to run PISSM using Docker without installing gfortran or other dependencies locally.

## Prerequisites

- **Docker** installed on your system ([Get Docker](https://docs.docker.com/get-docker/))
- **Docker Compose** (optional, included with Docker Desktop)

## Quick Start

### Option 1: Using Docker Compose (Recommended)

```bash
cd PISSM
docker-compose up
```

The container will start and launch PISSM, allowing you to browse directories.

### Option 2: Using Docker CLI

#### Build the Image

```bash
cd PISSM
docker build -t pissm:latest .
```

#### Run the Container

```bash
# Browse the root home directory
docker run -it --rm pissm:latest

# Browse a specific directory
docker run -it --rm pissm:latest /path/to/directory

# Browse your home directory with write access
docker run -it --rm -v ~:/home/user:rw pissm:latest /home/user

# Browse /tmp directory
docker run -it --rm pissm:latest /tmp
```

## Usage Examples

### Example 1: Simple Directory Browsing

```bash
# Browse root filesystem
docker run -it --rm pissm:latest /

# Browse /home
docker run -it --rm pissm:latest /home

# Browse /etc
docker run -it --rm pissm:latest /etc
```

### Example 2: Access Local Files

Mount your local directory into the container:

```bash
# Browse current directory (read-only)
docker run -it --rm -v $(pwd):/workspace:ro pissm:latest /workspace

# Browse current directory (read-write for file operations)
docker run -it --rm -v $(pwd):/workspace:rw pissm:latest /workspace

# On Windows (PowerShell)
docker run -it --rm -v ${pwd}:/workspace pissm:latest /workspace
```

### Example 3: Using Docker Compose with Custom Directory

Modify `docker-compose.yml`:

```yaml
services:
  pissm:
    build: .
    container_name: pissm-app
    stdin_open: true
    tty: true
    volumes:
      - /home:/home:ro
      - /root:/root:ro
      - /tmp:/tmp
      - ~/Desktop:/workspace  # Add your directory here
    environment:
      - TERM=xterm-256color
    command: /workspace  # Browse this directory
```

Then run:
```bash
docker-compose up
```

## Advanced Usage

### Running in Background

```bash
docker run -d --name pissm-bg pissm:latest /tmp
# Later, attach to it:
docker attach pissm-bg
# Or stop it:
docker stop pissm-bg
docker rm pissm-bg
```

### Interactive Shell Inside Container

```bash
docker run -it --rm pissm:latest /bin/bash
# Then run pissm manually:
/app/pissm/bin/pissm /home
```

### Building Debug Version

Create a `Dockerfile.debug` file:

```dockerfile
FROM ubuntu:24.04
WORKDIR /app
RUN apt-get update && apt-get install -y \
    gfortran build-essential make git \
    && rm -rf /var/lib/apt/lists/*
COPY . /app
WORKDIR /app/pissm
RUN make debug
ENTRYPOINT ["/app/pissm/bin/pissm-debug"]
CMD ["/root"]
```

Build and run:
```bash
docker build -f Dockerfile.debug -t pissm:debug .
docker run -it --rm pissm:debug /tmp
```

## Container Features

### Volumes (Data Sharing)

The container can access:
- Host filesystem via volume mounts
- Read-only mounts for safe browsing
- Read-write mounts for file operations

### Environment

- Full terminal support (xterm-256color)
- Interactive TTY enabled
- Signal handling for clean exit

### Resource Usage

- Minimal footprint once compiled
- Final image size: ~500MB (Ubuntu base) or ~150MB (Alpine)
- Memory usage: ~20-50MB while running

## Troubleshooting

### Permission Denied

If you get permission errors:
```bash
# Ensure Docker daemon is running
docker ps

# On Linux, you may need sudo
sudo docker run -it --rm pissm:latest /tmp

# Or add your user to docker group (Linux)
sudo usermod -aG docker $USER
newgrp docker
```

### Cannot Access Host Files

Make sure to mount volumes with full paths:
```bash
# Correct
docker run -it --rm -v /home/user/Documents:/work pissm:latest /work

# On Windows, use forward slashes
docker run -it --rm -v c:/Users/user/Documents:/work pissm:latest /work
```

### Terminal Display Issues

Set the TERM environment variable:
```bash
docker run -it --rm -e TERM=xterm-256color pissm:latest /tmp
```

### Out of Disk Space

Clean up Docker:
```bash
# Remove stopped containers
docker container prune

# Remove unused images
docker image prune

# Full cleanup (warning: removes all unused Docker data)
docker system prune -a
```

## Image Optimization

### Using Alpine Linux (Smaller Image)

Create `Dockerfile.alpine`:

```dockerfile
FROM alpine:latest
WORKDIR /app
RUN apk add --no-cache gfortran gcc g++ make musl-dev
COPY . /app
WORKDIR /app/pissm
RUN make release
ENTRYPOINT ["/app/pissm/bin/pissm"]
CMD ["/root"]
```

Build: `docker build -f Dockerfile.alpine -t pissm:alpine .`

Benefits:
- Smaller image size (~150MB vs 500MB)
- Faster startup
- Minimal dependencies

### Multi-stage Build (Production Optimized)

Create `Dockerfile.multistage`:

```dockerfile
# Build stage
FROM ubuntu:24.04 as builder
WORKDIR /app
RUN apt-get update && apt-get install -y \
    gfortran build-essential make \
    && rm -rf /var/lib/apt/lists/*
COPY . /app
WORKDIR /app/pissm
RUN make release

# Runtime stage
FROM ubuntu:24.04
RUN apt-get update && apt-get install -y libc6 && rm -rf /var/lib/apt/lists/*
COPY --from=builder /app/pissm/bin/pissm /usr/local/bin/pissm
ENTRYPOINT ["/usr/local/bin/pissm"]
CMD ["/root"]
```

Benefits:
- Much smaller final image (~100MB)
- Only runtime dependencies included
- Faster distribution

## Development Workflow

### Rebuild After Code Changes

```bash
# Rebuild the image
docker build -t pissm:latest .

# Run the updated version
docker run -it --rm pissm:latest /tmp
```

### Faster Rebuilds with Volume Mount

```bash
# Mount source code for live development
docker run -it --rm \
  -v $(pwd)/pissm/src:/app/pissm/src \
  -v $(pwd)/pissm/modules:/app/pissm/modules \
  pissm:latest /tmp
```

## CI/CD Integration

### GitHub Actions Example

Create `.github/workflows/docker-build.yml`:

```yaml
name: Build Docker Image

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: docker/setup-buildx-action@v1
      - uses: docker/build-push-action@v2
        with:
          context: .
          push: false
          tags: pissm:latest
```

## Documentation

- See [README.md](../README.md) for project overview
- See [CONTRIBUTING.md](../CONTRIBUTING.md) for development guidelines
- See [DEVELOPMENT.md](../DEVELOPMENT.md) for architecture details

## Support

For Docker-specific issues:
- Check [Docker documentation](https://docs.docker.com/)
- Review container logs: `docker logs <container-id>`
- Inspect running processes: `docker top <container-id>`

## Next Steps

1. **Build the image**: `docker build -t pissm:latest .`
2. **Run a container**: `docker run -it --rm pissm:latest /tmp`
3. **Navigate with vim keys**: `j/k` to move, `Enter` to open, `b` to go back
4. **Exit**: Press `q` or `Ctrl+C`

Enjoy PISSM without any local setup! 🐳
