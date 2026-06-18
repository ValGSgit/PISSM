# Quick Start: Running PISSM with Docker

If you don't have gfortran installed and don't want to install it, Docker is the easiest way to get PISSM running!

## What You Need

- **Docker** installed ([Get Docker here](https://docs.docker.com/get-docker/))
- **That's it!** No gfortran, no build tools, no compilation needed.

## 3 Ways to Run PISSM

### Option 1: Using the Helper Script (Easiest) 🎯

#### On Linux/macOS:
```bash
cd PISSM
chmod +x pissm.docker
./pissm.docker build    # Build the image (one-time)
./pissm.docker run      # Run PISSM
```

#### On Windows:
```cmd
cd PISSM
pissm.docker.bat build
pissm.docker.bat run
```

### Option 2: Using Docker Compose (Easy)

```bash
cd PISSM
docker-compose up
```

### Option 3: Using Docker CLI Directly

```bash
# Build (one-time)
cd PISSM
docker build -t pissm:latest .

# Run
docker run -it --rm pissm:latest /tmp
```

## Basic Usage

Once PISSM launches, use these keys:

| Key | Action |
|-----|--------|
| `j`/`↓` | Move down |
| `k`/`↑` | Move up |
| `Enter` | Open file/directory |
| `b`/`←` | Go back |
| `q` | Quit |
| `h` | Toggle hidden files |
| `/` | Search files |

## Browse Different Directories

```bash
# Browse /tmp
./pissm.docker run /tmp

# Browse /home
./pissm.docker run /home

# Browse any directory
./pissm.docker run /path/to/directory
```

## Advanced Usage

### Browse Your Local Files

```bash
# Linux/macOS - Browse current directory
docker run -it --rm -v $(pwd):/workspace pissm:latest /workspace

# Windows PowerShell - Browse current directory
docker run -it --rm -v ${pwd}:/workspace pissm:latest /workspace

# Windows CMD - Browse current directory
docker run -it --rm -v %cd%:/workspace pissm:latest /workspace
```

### Interactive Shell

```bash
./pissm.docker shell
# Then run PISSM manually:
/app/pissm/bin/pissm /tmp
```

### Debug Build (with runtime checks)

```bash
docker build -f Dockerfile.debug -t pissm:debug .
docker run -it --rm pissm:debug /tmp
```

## How It Works

1. **Dockerfile** - Contains Ubuntu with gfortran, builds PISSM automatically
2. **docker-compose.yml** - Orchestrates the container with nice defaults
3. **pissm.docker** - Helper script for easy commands
4. **.dockerignore** - Speeds up builds by excluding unnecessary files

## Troubleshooting

### "Docker daemon is not running"

Make sure Docker is running:
- **Linux**: `sudo systemctl start docker`
- **macOS**: Open Docker Desktop app
- **Windows**: Open Docker Desktop app

### "Permission denied" (Linux)

Add your user to docker group:
```bash
sudo usermod -aG docker $USER
newgrp docker
```

### "Cannot access files on my computer"

Use volume mounts:
```bash
docker run -it --rm -v /path/to/files:/workspace pissm:latest /workspace
```

### "Terminal looks broken"

Set the TERM variable:
```bash
docker run -it --rm -e TERM=xterm-256color pissm:latest /tmp
```

## Image Sizes

- **Default (Ubuntu)**: ~500 MB
- **Alpine version**: ~150 MB (faster, smaller)
- **Multi-stage build**: ~100 MB (production optimized)

For smaller images, see [DOCKER.md](DOCKER.md) for advanced build options.

## What's in the Container?

The Docker image includes:
- Ubuntu 24.04 base system
- gfortran compiler
- Build tools (make, gcc, g++)
- PISSM pre-compiled and ready to run

## Need Help?

See [DOCKER.md](DOCKER.md) for comprehensive Docker documentation including:
- Advanced volume mounting
- Multi-stage builds
- CI/CD integration
- Performance optimization
- And much more!

## Next Steps

1. Install Docker if you haven't: [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/)
2. Run: `./pissm.docker build`
3. Run: `./pissm.docker run`
4. Explore with `j`, `k`, and `Enter`!

---

Enjoy PISSM - no compilation, no dependencies, just Docker! 🐳
