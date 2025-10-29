class Fortress < Formula
  desc "Command-line file explorer written in modern Fortran with cd-on-exit"
  homepage "https://github.com/FortranGoingOnForty/fortress"
  url "https://github.com/FortranGoingOnForty/fortress/archive/v0.9.94.tar.gz"
  sha256 "96a040fbaeea73f0b4d5660ef2a454be7e6f27d11735a9a793be1c6c421c85c6"
  license "MIT"

  depends_on "fpm" => :build
  depends_on "gcc" # for gfortran
  depends_on "fzf"
  depends_on "git"

  def install
    # Build the binary
    system "fpm", "build", "--flag", "-O2"

    # Find the built binary (the gfortran_* directory name varies)
    built_binary = Dir["build/gfortran_*/app/fortress"].first
    raise "Could not find built binary" unless built_binary

    # Install the actual binary
    bin.install built_binary => "fortress-bin"

    # Create a simple wrapper script for basic usage
    # Note: For cd-on-exit feature, users still need to source the shell integration
    (bin/"fortress").write <<~EOS
      #!/bin/bash
      exec "#{bin}/fortress-bin" "$@"
    EOS

    # Install shell integration files for cd-on-exit feature
    (share/"fortress").install "fortress.sh"
    (share/"fortress").install "fortress.fish"

    # Install documentation
    doc.install "README.md"
    doc.install "USAGE.md" if File.exist?("USAGE.md")
  end

  def caveats
    <<~EOS
      FORTRESS has been installed!

      Basic usage:
        fortress  # Works immediately, all git features included

      === CD-ON-EXIT FEATURE (Optional) ===
      To enable the 'c' key to change your shell's directory:

      Bash/Zsh - Add to ~/.bashrc or ~/.zshrc:
        source #{HOMEBREW_PREFIX}/share/fortress/fortress.sh

      Fish - Add to ~/.config/fish/config.fish:
        source #{HOMEBREW_PREFIX}/share/fortress/fortress.fish

      Then restart your shell or source the config file.

      With shell integration, press 'c' on any directory to cd there and exit.

      Documentation: #{HOMEBREW_PREFIX}/share/doc/fortress/
    EOS
  end

  test do
    # Test that both binaries exist and are executable
    assert_predicate bin/"fortress-bin", :exist?
    assert_predicate bin/"fortress-bin", :executable?
    assert_predicate bin/"fortress", :exist?
    assert_predicate bin/"fortress", :executable?

    # Test that shell integration files exist
    assert_predicate share/"fortress/fortress.sh", :exist?
    assert_predicate share/"fortress/fortress.fish", :exist?
  end
end
