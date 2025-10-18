class Fortress < Formula
  desc "Command-line file explorer written in modern Fortran with cd-on-exit"
  homepage "https://github.com/FortranGoingOnForty/fortress"
  url "https://github.com/FortranGoingOnForty/fortress/archive/v0.1.0.tar.gz"
  sha256 "e38b495296e87f7bee7140ed662a37ee23f2f428e459b9c455f846f856014d4e"
  license "MIT"

  depends_on "fpm" => :build
  depends_on "gcc" # for gfortran

  def install
    # Build the binary
    system "fpm", "build", "--flag", "-O2"

    # Find the built binary (the gfortran_* directory name varies)
    built_binary = Dir["build/gfortran_*/app/fortress"].first
    raise "Could not find built binary" unless built_binary

    # Install the binary
    bin.install built_binary => "fortress-bin"

    # Install shell integration files
    (share/"fortress").install "fortress.sh"
    (share/"fortress").install "fortress.fish"

    # Install documentation
    doc.install "README.md"
    doc.install "USAGE.md" if File.exist?("USAGE.md")
  end

  def caveats
    <<~EOS
      FORTRESS has been installed!

      The binary is installed as 'fortress-bin', but you'll want to use
      the shell wrapper function for the cd-on-exit feature.

      === Bash/Zsh Setup ===
      Add to your ~/.bashrc or ~/.zshrc:
        source #{HOMEBREW_PREFIX}/share/fortress/fortress.sh

      Then restart your shell or run:
        source ~/.bashrc  # or ~/.zshrc

      === Fish Setup ===
      Add to your ~/.config/fish/config.fish:
        source #{HOMEBREW_PREFIX}/share/fortress/fortress.fish

      Then restart your shell or run:
        source ~/.config/fish/config.fish

      === Usage ===
      After setup, just run:
        fortress

      Press 'c' on any directory to cd there and exit.

      Documentation: #{HOMEBREW_PREFIX}/share/doc/fortress/
    EOS
  end

  test do
    # Test that the binary exists and runs
    assert_predicate bin/"fortress-bin", :exist?
    assert_predicate bin/"fortress-bin", :executable?

    # Test that shell integration files exist
    assert_predicate share/"fortress/fortress.sh", :exist?
    assert_predicate share/"fortress/fortress.fish", :exist?
  end
end
