#!/usr/bin/env bash
# Deploy fortress RPM to repos.musicsian.com
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$HOME/src/repos-musicsian-com"

# Get version from spec file
VERSION=$(grep "^Version:" "$SCRIPT_DIR/fortress.spec" | awk '{print $2}')
RELEASE=$(grep "^Release:" "$SCRIPT_DIR/fortress.spec" | awk '{print $2}' | cut -d'%' -f1)
NAME="fortress"

echo "======================================================================"
echo "  Deploying $NAME-$VERSION-$RELEASE to repos.musicsian.com"
echo "======================================================================"
echo ""

# Check if RPM exists
RPM_FILE=$(find ~/rpmbuild/RPMS -name "$NAME-$VERSION-$RELEASE*.rpm" | head -1)
if [ -z "$RPM_FILE" ]; then
    echo "❌ Error: RPM not found. Did you run ./build-rpm.sh first?"
    exit 1
fi

echo "▶ Found RPM: $RPM_FILE"

# Copy RPM to repo
echo "▶ Copying RPM to repo directory..."
cp -v "$RPM_FILE" "$REPO_DIR/RPMS/"

# Copy fortress.repo file if not already present
if [ ! -f "$REPO_DIR/fortress.repo" ]; then
    echo "▶ Creating fortress.repo file..."
    cp -v "$REPO_DIR/fuss.repo" "$REPO_DIR/fortress.repo"
    sed -i 's/fuss/fortress/g' "$REPO_DIR/fortress.repo"
    sed -i 's/Tree utility for dirty git files/Terminal file explorer in Fortran/' "$REPO_DIR/fortress.repo"
fi

echo ""
echo "✓ RPM deployed to: $REPO_DIR/RPMS/"
echo ""
echo "Next steps:"
echo "  1. cd $REPO_DIR"
echo "  2. git add RPMS/$NAME-*.rpm fortress.repo"
echo "  3. git commit -m 'Add fortress $VERSION-$RELEASE'"
echo "  4. ./deploy.sh  # This will sign, create metadata, and deploy to server"
echo ""
