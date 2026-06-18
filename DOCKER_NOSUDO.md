# Running PISSM Without Sudo on Linux

## The Problem

By default, Docker requires `sudo` to run, which means you can't just do:
```bash
docker run -it --rm pissm:latest /tmp  # ❌ Permission denied
```

## Solution 1: Add Your User to Docker Group (Recommended)

This allows you to run Docker without sudo. You only need to do this **once**.

### Step 1: Check if docker group exists

```bash
grep docker /etc/group
```

If nothing shows up, create the group (needs sudo from someone with admin):
```bash
sudo groupadd docker
```

### Step 2: Add your user to the docker group

```bash
sudo usermod -aG docker $USER
```

### Step 3: Apply the group changes

Choose **ONE** of these:

**Option A: Log out and log back in**
```bash
# Log out of your session completely and log back in
# This refreshes your group memberships
```

**Option B: Use newgrp (faster)**
```bash
newgrp docker
```

**Option C: Restart in this terminal**
```bash
exec su - $USER
```

### Step 4: Verify it works

```bash
docker ps
```

If this shows without errors (even if list is empty), you're good!

```bash
./pissm.docker build
./pissm.docker run /tmp
```

**That's it!** You can now use Docker without sudo. ✅

---

## Solution 2: Using `sudo` (If You Have It)

If adding to docker group isn't possible or you prefer to use sudo:

```bash
sudo ./pissm.docker build
sudo ./pissm.docker run /tmp
```

**Note:** The helper script will still work, but you'll need to use `sudo` with each command.

---

## Solution 3: Rootless Docker (Advanced)

For maximum security, Docker can run without root. This requires a specific Docker setup.

Check if your system has rootless Docker available:

```bash
dockerd-rootless-setuptool.sh install
```

Then use normally without `sudo`:
```bash
./pissm.docker build
./pissm.docker run /tmp
```

---

## Solution 4: Podman (Docker Alternative)

If Docker isn't available or you want an alternative, use **Podman** (works very similarly):

```bash
# Check if Podman is installed
podman --version

# If not installed on Ubuntu/Debian:
sudo apt install podman

# Or on RHEL/CentOS:
sudo yum install podman
```

Then use the same commands (Podman is a drop-in Docker replacement):

```bash
# Build with Podman
podman build -t pissm:latest .

# Run with Podman
podman run -it --rm pissm:latest /tmp
```

Podman doesn't require `sudo` or group configuration on most systems!

---

## Quick Troubleshooting

### "dial unix /var/run/docker.sock: permission denied"

This means Docker is running as root and your user can't access it.

**Fix:** Add user to docker group (Solution 1 above)

```bash
sudo usermod -aG docker $USER
newgrp docker
docker ps  # Test
```

### "Got permission denied while trying to connect to the Docker daemon"

Same issue - add to docker group:

```bash
sudo usermod -aG docker $USER
```

### "docker: command not found"

Docker isn't installed. Install it:

```bash
# Ubuntu/Debian
curl -fsSL https://get.docker.com -o get-docker.sh
bash get-docker.sh

# After installation, add yourself to docker group:
sudo usermod -aG docker $USER
newgrp docker
```

### Still get permission denied after adding to group?

Restart Docker daemon:

```bash
sudo systemctl restart docker

# Or restart your session
exit
# Log back in
```

---

## Working Examples

### After setting up docker group correctly:

```bash
# Build PISSM
./pissm.docker build

# Browse /tmp
./pissm.docker run /tmp

# Browse /home
./pissm.docker run /home

# Browse current directory
docker run -it --rm -v $(pwd):/workspace pissm:latest /workspace

# Get shell access
./pissm.docker shell

# Clean up
./pissm.docker clean
```

### With sudo (if that's your only option):

```bash
# Build PISSM
sudo ./pissm.docker build

# Browse /tmp  
sudo ./pissm.docker run /tmp

# Clean up
sudo ./pissm.docker clean
```

---

## Security Note

When adding your user to the docker group, understand that:

- ✅ You can run Docker without sudo
- ✅ More convenient for development
- ⚠️ Docker group users have broad privileges
- ⚠️ Only add trusted users to docker group

This is fine for personal development machines, but in shared environments, consider rootless Docker or Podman.

---

## Summary of Options

| Method | Pros | Cons | Steps |
|--------|------|------|-------|
| **Docker + Group** | Standard, convenient | One-time setup | 2-3 commands |
| **Docker + sudo** | Works anywhere | Type sudo each time | Just use `sudo` |
| **Rootless Docker** | Secure, no sudo | Complex setup | Special installation |
| **Podman** | No setup needed | Different tool | Install podman |

---

## Recommended Workflow

1. **First time:**
   ```bash
   sudo usermod -aG docker $USER
   newgrp docker
   ```

2. **Then always use:**
   ```bash
   ./pissm.docker build
   ./pissm.docker run /tmp
   ```

3. **No more sudo needed!** ✅

---

## Need Help?

### Check Docker daemon status

```bash
sudo systemctl status docker
sudo systemctl start docker   # If not running
```

### Debug group membership

```bash
# Check if user is in docker group
id -nG | grep docker

# If not listed, run:
sudo usermod -aG docker $USER
newgrp docker
id -nG | grep docker  # Should show now
```

### See all group members

```bash
getent group docker
```

### Test Docker access

```bash
# These should all work without sudo:
docker ps
docker images
docker run hello-world
```

---

## Still Having Issues?

### Option A: Use sudo temporarily

```bash
sudo ./pissm.docker build
sudo ./pissm.docker run /tmp
```

### Option B: Check with your sysadmin

If you're on a shared system:
- Ask sysadmin to add you to docker group
- Or ask them to install Podman instead
- Or ask them about rootless Docker

### Option C: Use Podman (usually works immediately)

```bash
podman run -it --rm pissm:latest /tmp
```

---

## Advanced: Custom Socket

If you need to use a custom Docker socket:

```bash
# Check socket location
echo $DOCKER_HOST

# Use different socket if available
DOCKER_HOST=unix:///run/podman/podman.sock ./pissm.docker build
```

---

Once you set up the docker group (one-time), you'll be able to use PISSM just like:

```bash
./pissm.docker run /tmp
```

No sudo needed! 🎉
