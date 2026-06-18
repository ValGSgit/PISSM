# PISSM on Linux Without Sudo - Complete Guide

## Your Situation

You're on a Linux machine where you don't have `sudo` access, and you want to run PISSM.

## Solutions (Pick One)

### Solution 1: Docker Group Access (Easiest) ✅

If you CAN run `sudo` once:

```bash
# One-time setup (need sudo just this once)
sudo usermod -aG docker $USER

# Apply the change
newgrp docker

# Verify
docker ps

# Now use PISSM forever without sudo
cd PISSM
./pissm.docker build
./pissm.docker run /tmp
```

**After this setup, you NEVER need sudo again.** ✅

---

### Solution 2: Docker with sudo Every Time

If you have `sudo` available:

```bash
# Every time you use Docker, add sudo
sudo docker build -t pissm:latest .
sudo docker run -it --rm pissm:latest /tmp

# Or with helper script
sudo ./pissm.docker build
sudo ./pissm.docker run /tmp
```

Not ideal, but works.

---

### Solution 3: Podman (No Setup Needed) ✅✅

If you have `sudo` just to install once:

```bash
# Install Podman (one-time, needs sudo)
sudo apt install podman    # Ubuntu/Debian
# or
sudo yum install podman    # RHEL/CentOS
# or
sudo dnf install podman    # Fedora

# Then use FOREVER without sudo
podman build -t pissm:latest .
podman run -it --rm pissm:latest /tmp
```

**Podman is like Docker but doesn't require group setup!**

---

### Solution 4: Traditional Build (If gfortran Available)

If you have gfortran installed:

```bash
cd pissm
make release      # or ./build.sh
./bin/pissm /tmp
```

No Docker needed, runs natively.

---

### Solution 5: Ask Your System Admin

If you can't do any of the above:

- "Can you add me to the docker group?"
- "Can you install Podman?" 
- "Can you install gfortran?"

Pick any one and you're good.

---

## Decision Tree

```
Can you run sudo?
├─ YES
│  ├─ Want to use sudo every time?
│  │  ├─ NO → Solution 1 (docker group, one-time setup)
│  │  └─ YES → Solution 2 (sudo each time)
│  └─ Want to use Podman instead?
│     └─ YES → Solution 3 (Podman, only sudo for install)
│
├─ NO (sudo not available)
│  ├─ Is gfortran installed?
│  │  └─ YES → Solution 4 (native build)
│  │
│  └─ Can you ask sysadmin?
│     └─ YES → Solution 5 (ask for docker group or podman)
│
└─ I give up → Ask for help in GitHub issues
```

---

## Step-by-Step Instructions

### For Solution 1: One-Time Docker Group Setup

**Step 1: Open terminal and run:**
```bash
sudo usermod -aG docker $USER
```

**Step 2: Apply the change (pick one):**

```bash
# Option A: Use newgrp
newgrp docker

# Option B: Log out and log back in
exit
# Log back in to your session

# Option C: Start a new shell
exec su - $USER
```

**Step 3: Verify it works:**
```bash
docker ps
```

If this shows a list (even if empty) without asking for password, you're good!

**Step 4: Build and run PISSM:**
```bash
cd PISSM
./pissm.docker build
./pissm.docker run /tmp
```

✅ **Done! Never use sudo with docker again.**

---

### For Solution 3: Podman Setup

**Step 1: Install Podman (one-time):**

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install podman

# RHEL/CentOS
sudo yum install podman

# Fedora
sudo dnf install podman
```

**Step 2: Build and run PISSM:**
```bash
cd PISSM

# Build (Podman works like Docker)
podman build -t pissm:latest .

# Run
podman run -it --rm pissm:latest /tmp
```

✅ **Done! Use Podman without sudo forever.**

---

## Important Notes

### Docker Group Security

When you add your user to the docker group:
- ✅ You can run Docker without sudo
- ✅ More convenient for development
- ⚠️ Docker users have broad system privileges
- ⚠️ Only add trusted users

This is fine for personal machines. Don't do this on shared systems without understanding the security implications.

### Podman vs Docker

| Feature | Docker | Podman |
|---------|--------|--------|
| **Sudo required** | Yes (by default) | No |
| **Group setup** | Yes | No |
| **Security** | Runs as root daemon | Daemonless, rootless |
| **Commands** | `docker ...` | `podman ...` |
| **Compatibility** | Standard | Drop-in replacement |

**Podman is essentially a more secure Docker alternative.**

---

## Troubleshooting

### "Still getting permission denied after newgrp"

```bash
# Restart Docker daemon
sudo systemctl restart docker

# Or log out completely and back in
exit
# Close terminal
# Open new terminal
# Log back in
```

### "docker: command not found"

Docker isn't installed. Either:
1. Install Docker (and add to group)
2. Use Podman instead (easier)
3. Use traditional build with gfortran

### "sudo: command not found"

You truly have no sudo. Options:
1. Use gfortran native build (if available)
2. Ask system admin for help
3. Use a different machine

### "Can I use both Docker and Podman?"

Yes! But PISSM will use whichever one is available. Podman is compatible with Docker commands.

---

## Quick Reference Commands

### Docker (after group setup)
```bash
./pissm.docker build
./pissm.docker run /tmp
./pissm.docker run /home
./pissm.docker clean
```

### Podman
```bash
podman build -t pissm:latest .
podman run -it --rm pissm:latest /tmp
podman run -it --rm pissm:latest /home
```

### Traditional (with gfortran)
```bash
cd pissm
make release
./bin/pissm /tmp
```

---

## Still Stuck?

### Check Your Current Setup

```bash
# Is Docker installed?
docker --version

# Can you run Docker?
docker ps

# Is Podman installed?
podman --version

# Can you run Podman?
podman ps

# Is gfortran installed?
gfortran --version

# What groups are you in?
id -nG
```

### Get Help

1. Check [DOCKER_NOSUDO.md](DOCKER_NOSUDO.md) for detailed Docker info
2. Check [DOCKER.md](DOCKER.md) for advanced Docker usage
3. See [CONTRIBUTING.md](CONTRIBUTING.md) if you want to build from source
4. Open a GitHub issue with your setup details

---

## My Recommendation

**Order by ease:**

1. **Easiest**: Solution 3 (Podman, if you can install)
   - Works immediately after install
   - No group setup needed
   - Better security
   
2. **Second easiest**: Solution 1 (Docker group)
   - One-time setup
   - Standard Docker works forever after
   
3. **Third**: Solution 2 (sudo each time)
   - Works anywhere
   - Annoying but functional

4. **Last resort**: Solution 4 (native build)
   - Requires gfortran
   - No Docker needed
   - Only if you have it

---

## Summary

You have **multiple ways** to run PISSM on Linux without sudo:

1. ✅ Docker group (one-time setup)
2. ✅ Podman (no setup needed beyond install)
3. ✅ Native build (if gfortran available)
4. ✅ Docker with sudo (not ideal but works)

**Pick the easiest one for your situation.** They all work! 🎉
