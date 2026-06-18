# 📋 PISSM Documentation Index

Navigate to the guide you need:

## 🚀 Getting Started

- **[README.md](README.md)** - Start here! Overview and quick start
- **[DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)** - Quick Docker setup (3 minutes)
- **[SETUP_COMPLETE.md](SETUP_COMPLETE.md)** - Everything that was improved

## 🐳 Docker Setup

- **[DOCKER.md](DOCKER.md)** - Comprehensive Docker guide (1000+ lines)
- **[DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)** - Quick 5-minute setup
- **[LINUX_NOSUDO_QUICK.md](LINUX_NOSUDO_QUICK.md)** - Run Docker without sudo (Linux)
- **[LINUX_NOSUDO_COMPLETE.md](LINUX_NOSUDO_COMPLETE.md)** - Detailed Linux without sudo guide

## 👨‍💻 Development

- **[CONTRIBUTING.md](CONTRIBUTING.md)** - How to contribute (~350 lines)
- **[DEVELOPMENT.md](DEVELOPMENT.md)** - Architecture and design patterns (~350 lines)
- **[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)** - Community standards

## 📚 Project Info

- **[CHANGELOG.md](CHANGELOG.md)** - What changed and future roadmap
- **[IMPROVEMENTS_SUMMARY.md](IMPROVEMENTS_SUMMARY.md)** - All improvements made to project

---

## 🎯 Choose Your Path

### I want to run PISSM right now

1. **Have Docker?** → [DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)
2. **Have gfortran?** → [README.md](README.md) Option 2
3. **On Linux without sudo?** → [LINUX_NOSUDO_QUICK.md](LINUX_NOSUDO_QUICK.md)
4. **Have Podman?** → Use Podman like Docker

### I want to understand the project

1. Start: [README.md](README.md)
2. Then: [DEVELOPMENT.md](DEVELOPMENT.md)
3. Deep dive: [IMPROVING_SUMMARY.md](IMPROVEMENTS_SUMMARY.md)

### I want to contribute

1. Read: [CONTRIBUTING.md](CONTRIBUTING.md)
2. Read: [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)
3. Read: [DEVELOPMENT.md](DEVELOPMENT.md)

### I have Docker/Linux issues

- Docker problems: [DOCKER.md](DOCKER.md) or [DOCKER_NOSUDO_COMPLETE.md](LINUX_NOSUDO_COMPLETE.md)
- Build problems: [CONTRIBUTING.md](CONTRIBUTING.md)
- Linux without sudo: [LINUX_NOSUDO_COMPLETE.md](LINUX_NOSUDO_COMPLETE.md)

---

## 📁 Project Structure

```
PISSM/
├── README.md                          ← Start here
├── pissm/                             ← Main project
│   ├── Makefile
│   ├── build.sh
│   ├── modules/
│   │   ├── constants_mod.f90
│   │   ├── types_mod.f90
│   │   └── globals_mod.f90
│   └── src/
│       ├── utilities.f90
│       ├── main.f90
│       └── ...
│
├── Docker files
│   ├── Dockerfile
│   ├── docker-compose.yml
│   ├── .dockerignore
│   ├── pissm.docker              ← Use this (Linux/macOS)
│   └── pissm.docker.bat          ← Use this (Windows)
│
└── Documentation
    ├── DOCKER_QUICKSTART.md
    ├── DOCKER.md
    ├── LINUX_NOSUDO_QUICK.md
    ├── LINUX_NOSUDO_COMPLETE.md
    ├── CONTRIBUTING.md
    ├── DEVELOPMENT.md
    ├── CODE_OF_CONDUCT.md
    ├── CHANGELOG.md
    └── SETUP_COMPLETE.md
```

---

## 🔥 Quick Reference

### Run PISSM with Docker

```bash
# Build
./pissm.docker build

# Run
./pissm.docker run /tmp
```

### Build Manually

```bash
cd pissm
make release
./bin/pissm /tmp
```

### Using Podman

```bash
podman build -t pissm:latest .
podman run -it --rm pissm:latest /tmp
```

---

## ✨ Navigation Keys (Once Running)

| Key | Action |
|-----|--------|
| `j`/`↓` | Move down |
| `k`/`↑` | Move up |
| `Enter` | Open file/directory |
| `b`/`←` | Go back |
| `q` | Quit |
| `gg` | Jump to start |
| `G` | Jump to end |
| `h` | Toggle hidden files |
| `/` | Search |

---

## 🆘 Troubleshooting Quick Links

| Issue | Solution |
|-------|----------|
| Docker permission denied | [LINUX_NOSUDO_QUICK.md](LINUX_NOSUDO_QUICK.md) |
| Docker not running | [DOCKER.md](DOCKER.md#troubleshooting) |
| Build fails | [CONTRIBUTING.md](CONTRIBUTING.md#building-the-project) |
| Linux no sudo | [LINUX_NOSUDO_COMPLETE.md](LINUX_NOSUDO_COMPLETE.md) |
| General help | [CONTRIBUTING.md](CONTRIBUTING.md#questions-or-need-help) |

---

## 📊 Documentation Stats

| Document | Size | Purpose |
|-----------|------|---------|
| DOCKER_QUICKSTART.md | ~200 lines | 5-minute Docker setup |
| DOCKER.md | ~1000 lines | Comprehensive Docker guide |
| CONTRIBUTING.md | ~350 lines | Contributor guidelines |
| DEVELOPMENT.md | ~350 lines | Architecture & patterns |
| LINUX_NOSUDO_COMPLETE.md | ~300 lines | Detailed Linux setup |
| LINUX_NOSUDO_QUICK.md | ~50 lines | Quick Linux reference |
| CODE_OF_CONDUCT.md | ~100 lines | Community standards |
| CHANGELOG.md | ~150 lines | Changes & roadmap |

**Total:** ~2500 lines of comprehensive documentation

---

## 🎓 Learning Path

**5 minutes**: Read [README.md](README.md) and [DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)

**15 minutes**: Get PISSM running with Docker

**30 minutes**: Run PISSM and explore the interface

**1 hour**: Read [DEVELOPMENT.md](DEVELOPMENT.md) to understand architecture

**2 hours**: Read [CONTRIBUTING.md](CONTRIBUTING.md) to understand how to contribute

---

## 💡 Pro Tips

1. **On Linux?** See [LINUX_NOSUDO_QUICK.md](LINUX_NOSUDO_QUICK.md) for no-sudo setup
2. **Want to contribute?** Read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) first
3. **Building on new system?** See [CONTRIBUTING.md](CONTRIBUTING.md) prerequisites
4. **Docker issues?** Check [DOCKER.md](DOCKER.md) troubleshooting section
5. **Stuck?** Check README → DOCKER_QUICKSTART → LINUX_NOSUDO_QUICK (in that order)

---

## 📞 Get Help

1. **First**: Check README.md for quick answers
2. **Then**: Look for your issue in the relevant guide
3. **Still stuck?** Check [CONTRIBUTING.md#questions-or-need-help](CONTRIBUTING.md#questions-or-need-help)

---

Last updated: 2026-06-18 | All documentation reviewed and current
