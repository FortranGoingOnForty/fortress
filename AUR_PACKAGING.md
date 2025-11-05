# AUR Packaging Guide for FORTRESS

This guide explains how to package FORTRESS for the Arch User Repository (AUR).

## Files Needed for AUR

The following files are required in the AUR repository:

1. **PKGBUILD** - Build script that describes how to build and install the package
2. **.SRCINFO** - Metadata file generated from PKGBUILD
3. **fortress.install** - Post-install messages and scripts

## Preparing for AUR Submission

### 1. Create a Release on GitHub

First, create a tagged release on GitHub:

```bash
# Tag the release
git tag -a v0.1.0 -m "Release v0.1.0"
git push origin v0.1.0
```

Then create a release on GitHub using this tag.

### 2. Generate Checksum

Download the release tarball and generate its SHA256 checksum:

```bash
# Download the release
wget https://github.com/FortranGoingOnForty/fortress/archive/v0.1.0.tar.gz

# Generate checksum
sha256sum v0.1.0.tar.gz
```

Update the `sha256sums` line in PKGBUILD with this value.

### 3. Test the PKGBUILD

Before submitting to AUR, test the build locally:

```bash
# Install dependencies (if not already installed)
sudo pacman -S fpm gcc-fortran base-devel

# Test build
makepkg -sf

# Test installation
sudo pacman -U fortress-0.1.0-1-x86_64.pkg.tar.zst

# Test the installed package
fortress
```

### 4. Generate .SRCINFO

The .SRCINFO file must be generated from PKGBUILD:

```bash
makepkg --printsrcinfo > .SRCINFO
```

**Important**: Regenerate .SRCINFO every time you update PKGBUILD!

### 5. Submit to AUR

If this is your first time:

```bash
# Clone the AUR repository (will be empty initially)
git clone ssh://aur@aur.archlinux.org/fortress.git aur-fortress
cd aur-fortress

# Copy the required files
cp ../PKGBUILD .
cp ../fortress.install .
makepkg --printsrcinfo > .SRCINFO

# Commit and push
git add PKGBUILD fortress.install .SRCINFO
git commit -m "Initial import of fortress v0.1.0"
git push
```

For updates:

```bash
cd aur-fortress

# Update PKGBUILD (bump pkgver, update sha256sums, etc.)
# Then regenerate .SRCINFO
makepkg --printsrcinfo > .SRCINFO

# Commit and push
git add PKGBUILD .SRCINFO
git commit -m "Update to v0.2.0"
git push
```

## How Installation Works

When users install fortress from AUR:

1. **Binary Installation**: The executable is installed as `/usr/bin/fortress-bin`

2. **Shell Integration** (automatic for most shells):
   - **Bash**: Auto-loaded from `/etc/profile.d/fortress.sh` on shell startup
   - **Fish**: Auto-loaded from `/usr/share/fish/vendor_functions.d/fortress.fish`
   - **Zsh**: Users need to add `source /usr/share/fortress/fortress.sh` to `~/.zshrc`

3. **Usage**: Users just run `fortress` and the cd-on-exit feature works automatically

## Testing Shell Integration

After installation, test each shell:

```bash
# Bash
bash
fortress  # Should work, press 'c' to cd

# Fish
fish
fortress  # Should work, press 'c' to cd

# Zsh (after sourcing)
zsh
source /usr/share/fortress/fortress.sh
fortress  # Should work, press 'c' to cd
```

## Maintaining the AUR Package

### Version Updates

1. Update `pkgver` in PKGBUILD
2. Update `sha256sums` with new release checksum
3. Test build: `makepkg -sf`
4. Regenerate .SRCINFO: `makepkg --printsrcinfo > .SRCINFO`
5. Commit and push to AUR

### Common Issues

**fpm not in official repos**: Users need to install `fpm` from AUR first:
```bash
yay -S fpm
```

**Build fails**: Check that all makedepends are correct and the build() function works

**Shell integration doesn't work**: Verify files are installed to correct locations and users have restarted their shell

## Resources

- [AUR Submission Guidelines](https://wiki.archlinux.org/title/AUR_submission_guidelines)
- [PKGBUILD Manual](https://wiki.archlinux.org/title/PKGBUILD)
- [Creating Packages](https://wiki.archlinux.org/title/Creating_packages)
