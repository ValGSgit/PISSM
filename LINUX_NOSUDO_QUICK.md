# Quick Setup: Run PISSM Without Sudo on Linux

## TL;DR - Do This Once

```bash
# Add yourself to docker group
sudo usermod -aG docker $USER

# Apply the change
newgrp docker

# Verify it worked
docker ps
```

## Now Use PISSM

```bash
cd PISSM
./pissm.docker build    # Build once
./pissm.docker run /tmp # Browse /tmp
```

**Done!** No sudo needed from now on. ✅

---

## If You Don't Have Sudo

Use **Podman** instead (works out of the box):

```bash
# Install (if not already installed)
sudo apt install podman    # Ubuntu/Debian - this one-time sudo is needed
# or
sudo yum install podman    # RHEL/CentOS

# Then use exactly like Docker (no sudo needed)
podman build -t pissm:latest .
podman run -it --rm pissm:latest /tmp
```

---

## Troubleshooting

### "Permission denied"

```bash
# Fix: Add to docker group (one-time)
sudo usermod -aG docker $USER
newgrp docker
```

### "Still getting permission denied"

```bash
# Restart Docker daemon
sudo systemctl restart docker

# Or restart your session
exit
# Log back in
```

### "sudo: command not found"

You don't have sudo. Use **Podman** instead (see above).

---

For detailed help, see [DOCKER_NOSUDO.md](DOCKER_NOSUDO.md)
