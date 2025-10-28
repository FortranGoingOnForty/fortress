Name:           fortress
Version:        0.9.93
Release:        1%{?dist}
Summary:        A command-line file explorer written in modern Fortran with cd-on-exit

License:        MIT
URL:            https://github.com/FortranGoingOnForty/fortress
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  gfortran >= 10.0
BuildRequires:  gcc
BuildRequires:  fpm
Requires:       glibc
Requires:       gcc-libs
Requires:       fzf
Requires:       git

%description
FORTRESS is a command-line file explorer written in modern Fortran with fzf integration.
It provides a dual-pane interface inspired by Ranger/Midnight Commander with intuitive
keyboard navigation and a unique cd-on-exit feature that allows you to navigate your
shell to any directory you select in the explorer.

Features:
- Dual-pane display (parent dir 30%% | current dir 70%%)
- Real filesystem navigation with directory reading
- Smart selection memory - remembers position when navigating
- Visual hierarchy with dimmed parent pane and active current pane
- Color-coded files (directories, executables, dotfiles, regular files)
- Smooth updates with no flashing, selective redraws
- Arrow key navigation for intuitive movement
- Full-width selection bar with clean highlighting
- CD on exit - press 'c' to navigate your shell to selected directory

%prep
%autosetup

%build
fpm build --flag "-O2"

%install
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_datadir}/%{name}
mkdir -p %{buildroot}%{_datadir}/fish/vendor_functions.d
mkdir -p %{buildroot}%{_sysconfdir}/profile.d
mkdir -p %{buildroot}%{_docdir}/%{name}

# Install the binary
install -Dm755 build/gfortran_*/app/fortress %{buildroot}%{_bindir}/fortress-bin

# Install shell integration files to shared location
install -Dm644 fortress.sh %{buildroot}%{_datadir}/%{name}/fortress.sh
install -Dm644 fortress.fish %{buildroot}%{_datadir}/%{name}/fortress.fish

# Install bash integration to profile.d (auto-sourced on login)
install -Dm644 fortress.sh %{buildroot}%{_sysconfdir}/profile.d/fortress.sh

# Install fish integration to vendor functions (auto-loaded)
install -Dm644 fortress.fish %{buildroot}%{_datadir}/fish/vendor_functions.d/fortress.fish

# Install documentation
install -Dm644 README.md %{buildroot}%{_docdir}/%{name}/README.md
if [ -f USAGE.md ]; then
    install -Dm644 USAGE.md %{buildroot}%{_docdir}/%{name}/USAGE.md
fi

%files
%license LICENSE
%doc README.md
%{_bindir}/fortress-bin
%{_datadir}/%{name}/fortress.sh
%{_datadir}/%{name}/fortress.fish
%config(noreplace) %{_sysconfdir}/profile.d/fortress.sh
%{_datadir}/fish/vendor_functions.d/fortress.fish
%{_docdir}/%{name}/README.md

%post
cat <<'EOF'

====================================================================
  FORTRESS - Terminal File Explorer
====================================================================

  The 'fortress' command is now available!

  Shell integration (cd-on-exit) has been installed:
    • Bash: Auto-loaded via /etc/profile.d/fortress.sh
    • Fish: Auto-loaded via vendor_functions.d
    • Zsh:  Add to ~/.zshrc:
            source %{_datadir}/%{name}/fortress.sh

  You may need to restart your shell or run:
    source /etc/profile.d/fortress.sh  # bash

  Usage:
    fortress  - Start the file explorer
    Press 'c' on a directory to cd there and exit

  Documentation: %{_docdir}/%{name}/

====================================================================

EOF

%changelog
* Mon Oct 27 2025 mfw <espadon@outlook.com> - 0.9.93-1
- Fix shell integration to find fortress-bin in PATH
- Improves Homebrew compatibility on macOS

* Mon Oct 27 2025 mfw <espadon@outlook.com> - 0.9.92-1
- Add favorites functionality with * and 8 keybinds
- Add jump to ~ and / with keybinds
- Fix display alignment issues with pipes

* Sun Oct 26 2025 mfw <espadon@outlook.com> - 0.9.91-1
- Fix runtime errors on switching terminal modes
- Improve terminal mode handling

* Sat Oct 25 2025 mfw <espadon@outlook.com> - 0.9.9-1
- Fix hashes appearing in copy status for multi-select operations
- UI improvements for multi-select mode

* Sat Oct 25 2025 mfw <espadon@outlook.com> - 0.9.8-1
- Add multi-select with space key
- Add shift+arrow for range selection
- Prevent selection of . and .. entries
- Major code cleanup (removed lib_modules directory)

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.5-1
- Add 'r' key to remove files
- Add copy, cut, paste functionality
- Recursive move for directories
- Stage directories with move mode
- Exit move mode with 'q'
- Smart duplicate suffix on paste
- Fix suffix concatenation

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.9.2-1
- Add 'n' key to rename files
- Add '.' key to toggle dotfiles visibility
- Add 'v' key to start move mode
- Add git fetch/pull with incoming change indicators
- Show dirty indicator on directories
- Fix buffer issues in move mode
- Various move behavior improvements

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.9.0-1
- Cache git operations for better performance
- Optimize file attribute checking
- Fix directory entry navigation on arrow keys
- Work in progress: diff keybinding

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.8.0-1
- Add 'o' key to open files with $EDITOR
- Platform-dependent features
- Enhanced file operations

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.7.0-1
- Add git push functionality
- Add git tag creation and management
- Enhanced git operations

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.6.0-1
- Add src/ui/preview.f90 module
- Code cleanup and documentation improvements

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.5.0-1
- Major refactor: modularize codebase into src/ directory
- Add branch info to git status bar
- Make git integration reactive
- Improved code organization and maintainability

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.4.0-2
- Add git as runtime dependency for git integration features

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.4.0-1
- Add git integration features with status display
- Add 'U' key to unstage staged files in listing
- Fix terminal size handling issues
- Various bug fixes and improvements

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.2.0-1
- Add fzf integration for fuzzy file search
- Fix cursor clamping to prevent disappearing on ..
- Fix cd-on-exit regression
- Various cursor fixes and improvements
- Add fzf as runtime dependency

* Fri Oct 18 2025 mfw <espadon@outlook.com> - 0.1.0-1
- Initial RPM release
- Dual-pane file explorer with Fortran implementation
- CD-on-exit functionality with shell integration
- Color-coded file display with visual hierarchy
- Smart selection memory and arrow key navigation
- Automatic shell integration for bash and fish
- Manual setup required for zsh
