#!/usr/bin/env bash
# Build script for creating fortress RPM package
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Get version from spec file
VERSION=$(grep "^Version:" fortress.spec | awk '{print $2}')
RELEASE=$(grep "^Release:" fortress.spec | awk '{print $2}' | cut -d'%' -f1)
NAME="fortress"

echo "======================================================================"
echo "  Building $NAME-$VERSION-$RELEASE RPM"
echo "======================================================================"
echo ""

# Create rpmbuild directories
mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}

echo "▶ Creating source tarball..."
# Create source tarball (excluding build artifacts and git)
tar --exclude='.git' \
    --exclude='build' \
    --exclude='*.rpm' \
    --exclude='*.tar.gz' \
    --exclude='rpmbuild' \
    --transform "s,^.,$NAME-$VERSION," \
    -czf ~/rpmbuild/SOURCES/$NAME-$VERSION.tar.gz \
    .

echo "✓ Tarball created: ~/rpmbuild/SOURCES/$NAME-$VERSION.tar.gz"

echo "▶ Copying spec file..."
cp fortress.spec ~/rpmbuild/SPECS/

echo "▶ Building RPM package..."
rpmbuild -ba ~/rpmbuild/SPECS/fortress.spec

echo ""
echo "======================================================================"
echo "  Build Complete!"
echo "======================================================================"
echo ""
echo "RPMs created in:"
ls -lh ~/rpmbuild/RPMS/*/$NAME-*.rpm 2>/dev/null || echo "  (No binary RPMs found)"
echo ""
echo "SRPMs created in:"
ls -lh ~/rpmbuild/SRPMS/$NAME-*.rpm 2>/dev/null || echo "  (No source RPMs found)"
echo ""
