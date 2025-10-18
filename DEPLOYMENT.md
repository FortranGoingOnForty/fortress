# Fortress Deployment Guide

This guide covers deploying fortress to AUR, Homebrew, and repos.musicsian.com.

## Prerequisites

### For RPM builds
- `rpmbuild` (install via `dnf install rpm-build`)
- `fpm` (Fortran Package Manager) - install via `cargo install fpm`
- `createrepo_c` (for repository metadata)
- GPG key configured for signing

### For AUR
- AUR account with SSH key configured
- `makepkg` (part of Arch Linux base-devel)
- `updpkgsums` (to update checksums)

### For Homebrew
- Homebrew tap repository (or PR to homebrew-core)
- Access to create GitHub releases

## 1. AUR Deployment

The AUR package is already configured in this repository.

### Initial Setup (One-time)

```bash
# Clone your AUR repository
git clone ssh://aur@aur.archlinux.org/fortress.git fortress-aur
cd fortress-aur

# Copy files from fortress repo
cp /path/to/fortress/{PKGBUILD,.SRCINFO,fortress.install} .
```

### Deploying Updates

```bash
cd fortress-aur

# Update version in PKGBUILD
vim PKGBUILD  # Update pkgver and pkgrel

# Generate checksums
updpkgsums

# Generate .SRCINFO
makepkg --printsrcinfo > .SRCINFO

# Test build locally
makepkg -si

# Push to AUR
git add PKGBUILD .SRCINFO fortress.install
git commit -m "Update fortress to version X.Y.Z"
git push
```

**Notes:**
- Shell integration is automatic for bash and fish
- Zsh users need to manually source `/usr/share/fortress/fortress.sh`
- The `fortress.install` file shows post-install messages to users

## 2. Homebrew Deployment

Homebrew formulas can be deployed via a tap or submitted to homebrew-core.

### Option A: Personal Tap (Easier)

```bash
# Create a tap repository (first time only)
# Repository name must be: homebrew-<tapname>
# Example: homebrew-fortran

git clone https://github.com/yourusername/homebrew-fortran
cd homebrew-fortran

# Copy the formula
cp /path/to/fortress/fortress.rb Formula/

# Create a GitHub release first with the tarball
# Update the sha256 in fortress.rb:
shasum -a 256 fortress-0.1.0.tar.gz

# Commit and push
git add Formula/fortress.rb
git commit -m "Add fortress 0.1.0"
git push

# Users install with:
# brew tap yourusername/fortran
# brew install fortress
```

### Option B: Submit to homebrew-core (Official)

1. Fork https://github.com/Homebrew/homebrew-core
2. Add `fortress.rb` to `Formula/`
3. Submit a PR following Homebrew guidelines
4. Wait for review and approval

**Notes:**
- Homebrew **cannot** automatically modify user dotfiles
- Users must manually source the shell integration files
- The caveats block provides clear installation instructions
- Shell integration files are installed to `#{HOMEBREW_PREFIX}/share/fortress/`

### Updating the Formula

```bash
# Update version and sha256
vim Formula/fortress.rb

# Test locally
brew install --build-from-source ./Formula/fortress.rb
brew test fortress
brew audit --strict fortress

# Commit and push
git add Formula/fortress.rb
git commit -m "fortress 0.2.0"
git push
```

## 3. RPM Repository (repos.musicsian.com)

### Build the RPM

```bash
cd /home/espadon/src/fortress

# Ensure fpm is installed
cargo install fpm  # If not already installed

# Build the RPM
./build-rpm.sh
```

This script will:
1. Create a source tarball
2. Build the RPM using rpmbuild
3. Output RPMs to `~/rpmbuild/RPMS/`

### Deploy to Repository

```bash
cd /home/espadon/src/fortress

# Copy RPM to repo and prepare deployment
./deploy-to-repo.sh

# Change to repo directory
cd ~/src/repos-musicsian-com

# Add and commit
git add RPMS/fortress-*.rpm fortress.repo
git commit -m "Add fortress 0.1.0-1"
git push

# Deploy to server (signs RPMs, creates metadata, updates site)
./deploy.sh
```

The `deploy.sh` script will:
1. Stage files to a timestamped build directory
2. Sign all RPM packages with GPG
3. Generate repository metadata with createrepo_c
4. Sign repository metadata
5. Deploy to `/var/www/repos.musicsian.com/releases/`
6. Update the `current` symlink
7. Reload nginx
8. Test the deployment

### User Installation

Users can install from your repo with:

```bash
# Add the repository
sudo curl -o /etc/yum.repos.d/fortress.repo https://repos.musicsian.com/fortress.repo

# Install fortress
sudo dnf install fortress

# The shell integration is automatic for bash and fish
# Zsh users need to add to ~/.zshrc:
source /usr/share/fortress/fortress.sh
```

## Shell Integration Comparison

| Package Manager | Bash | Fish | Zsh |
|----------------|------|------|-----|
| AUR | ✅ Auto | ✅ Auto | ⚠️ Manual |
| Homebrew | ⚠️ Manual | ⚠️ Manual | ⚠️ Manual |
| RPM | ✅ Auto | ✅ Auto | ⚠️ Manual |

**Auto**: Shell integration is automatically set up during installation
**Manual**: User must manually source the integration file

## Version Bumping Checklist

When releasing a new version:

- [ ] Update version in `fpm.toml`
- [ ] Update version in `fortress.spec` (RPM)
- [ ] Update version in `PKGBUILD` (AUR)
- [ ] Update version in `fortress.rb` (Homebrew)
- [ ] Create GitHub release with tarball
- [ ] Update SHA256 checksums:
  - [ ] `updpkgsums` for AUR
  - [ ] Manual update in Homebrew formula
- [ ] Update changelog in `fortress.spec`
- [ ] Test build on all platforms
- [ ] Deploy in order: AUR → RPM → Homebrew

## Troubleshooting

### RPM Build Fails - "fpm: command not found"

```bash
cargo install fpm
# Add to PATH if needed:
export PATH="$HOME/.cargo/bin:$PATH"
```

### AUR Build Fails - Checksum Mismatch

```bash
updpkgsums  # Regenerate checksums
makepkg --printsrcinfo > .SRCINFO
```

### Homebrew Install Fails - Binary Not Found

Check that the glob pattern in `fortress.rb` matches your build output:
```ruby
built_binary = Dir["build/gfortran_*/app/fortress"].first
```

### Shell Integration Not Working

Make sure users restart their shell or source the integration file:
```bash
# Bash
source /etc/profile.d/fortress.sh  # RPM
source /usr/share/fortress/fortress.sh  # AUR
source $(brew --prefix)/share/fortress/fortress.sh  # Homebrew

# Fish
source /usr/share/fish/vendor_functions.d/fortress.fish  # RPM/AUR
source $(brew --prefix)/share/fortress/fortress.fish  # Homebrew
```

## Automation Ideas

Consider setting up:
- GitHub Actions to automatically build and test on releases
- Automatic AUR updates via CI/CD
- Homebrew bump-formula-pr for version updates
- RPM builds in CI with artifact upload

## Notes on Homebrew Shell Integration

Homebrew **cannot** automatically set up shell integration because:
1. Homebrew formulas run in a sandbox
2. Writing to user home directories is not allowed
3. System-wide profile.d directories vary by OS (macOS vs Linux)

This is why the `caveats` block is important - it provides clear instructions users will see after installation.

## License

MIT - See LICENSE file for details
